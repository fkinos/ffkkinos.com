+++
date = "2024-06-08T00:00:00-03:00"
title = "Less Parentheses"
+++

Common Lisp and other Lisp dialects are often dismissed as having too many parentheses,
while I don't think parentheses are an issue (in fact I think they're a feature),
that comment has some truth in it.

Let's see how Common Lisp would look like with less parentheses!

## Nesting
A key part of this experiment is nesting property lists so they match what _let_, _cond_, and other forms expect.

```
(defun nest (plist)
  "Nests a property-like list.

Example:

  (nest '(x 1 y 2))
  ; =>
  '((x 1) (y 2))

"
  (loop :for (key value) :on plist :by 'cddr
        :collect (list key value)))
```

## Let
The _let_ form requires an extra pair of parens around each binding,
this contributes to the notion of "too much parens".

I would like to remove those extra parens,
making the _let_ form something like this:
```
(let (x 10
      y 20)
  (* x y))
```

We can implement that with the following macro making use of the _nest_ function we created earlier:
```
(defmacro @let (bindings &body body)
  "Like let but doesn't require extra nesting around each binding.

Example:

  (@let (x 1
         y 2)
    (+ x y))
  => 3

"
  `(let ,(nest bindings)
     ,@body))
```

I've decided to use this '@' prefix to the new forms since they imply this 'splice' action when used inside macros.

## Cond

The same can be done for the _cond_ form with a similar macro:
```
(defmacro @cond (&rest clauses)
  "Like cond but doesn't require extra nesting around each clause.

Example:

  (defparameter x 10)

  (@cond
    (= x 5) :error
    (= x 10) :success
    t :unknown)

"
  `(cond
     ,@(nest clauses)))
```

## Case
The macro for the _case_ form is very similar as well:
```
(defmacro @case (keyform &body cases)
  "Like case but doesn't require extra nesting around each case.

Example:

  (defparameter x 10)

  (@case x
    10 :ten
    100 :hundred
    t :invalid)

"
  `(case ,keyform
     ,@(nest cases)))
```

## Typecase
The macro for the _typecase_ is _very_ similar as well, can you see the pattern?
```
(defmacro @typecase (keyform &body cases)
  "Like typecase but doesn't require extra nesting around each case.

Example:

  (defparameter x 10)

  (@typecase x
    integer (* x 10)
    string (format t "~a!~%" x))
  ; =>
  100

"
  `(typecase ,keyform
     ,@(nest cases)))
```

## Conclusion
With the _nest_ function doing all the heavy lifting and these simple macros,
where the docstring is bigger than the actual implementation,
we created a new mini lisp dialect. What else could we make with the use of macros?

Let me know what do you think about this!

## Source Code
Here's a full package that implements this macros for easier testing:
```
(in-package #:cl-user)
(defpackage lepar
  (:use #:cl))
(in-package #:lepar)


(defun nest (list)
  "Nests a property-like list.

Example:

  (nest '(x 1 y 2))
  ; =>
  '((x 1) (y 2))

"
  (loop :for (key value) :on list :by 'cddr
        :collect (list key value)))

(defmacro @let (bindings &body body)
  "Like let but doesn't require extra nesting around each binding.

Example:

  (@let (x 1
         y 2)
    (+ x y))
  => 3

"
  `(let ,(nest bindings)
     ,@body))


(defmacro @cond (&rest clauses)
  "Like cond but doesn't require extra nesting around each clause.

Example:

  (defparameter x 10)

  (@cond
    (= x 5) :error
    (= x 10) :success
    t :unknown)

"
  `(cond
     ,@(nest clauses)))


(defmacro @case (keyform &body cases)
  "Like case but doesn't require extra nesting around each case.

Example:

  (defparameter x 10)

  (@case x
    10 :ten
    100 :hundred
    t :invalid)

"
  `(case ,keyform
     ,@(nest cases)))


(defmacro @typecase (keyform &body cases)
  "Like typecase but doesn't require extra nesting around each case.

Example:

  (defparameter x 10)

  (@typecase x
    integer (* x 10)
    string (format t "~a!~%" x))
  ; =>
  100

"
  `(typecase ,keyform
     ,@(nest cases)))
```
