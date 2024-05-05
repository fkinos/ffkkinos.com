+++
date = "2024-05-04T00:00:00-03:00"
title = "Fun With Macros"
+++

This note is very much inspired by [steve losh's](https://stevelosh.com/) Fun With Macros blog posts.

Macros can be fun and sometimes helpful too,
let's play around with some macros I created in the last week or so!

## Hash Table

By default, Common Lisp doesn't provide literal hash tables,
we can create a simple macro that let's do something similar:

```
(defmacro dict (&body body)
  "Creates a hash table from a plist-like BODY.

Example:

  (dict :a 1
        :b 2
        :c 3)
  ; =>
  (LET ((#:G239 (MAKE-HASH-TABLE :TEST 'EQUAL)))
    (SETF (GETHASH :A #:G239) 1)
    (SETF (GETHASH :B #:G239) 2)
    (SETF (GETHASH :C #:G239) 3)
    #:G239)

"
  (let ((ht (gensym)))
    `(let ((,ht (make-hash-table :test 'equal)))
        ,@(loop :for (key value) :on body :by 'cddr
                :collect
                `(setf (gethash ,key , ht) ,value))
        ,ht)))
```

We first create a new symbol with *gensym*,
*gensym* guarantees that this symbol will not be used ever again throughout our program,
this is pretty useful to avoid name clashs between symbols we define, as the macro's author,
and symbols the user defines and passes to our macro.

Right after that we assign a new hash table to the symbol we generated and loop through the *body*.
We loop through *body*, that should look something like this:
```
(:a 1 :b 2 :c 3 ...)
```
Extracting those key-value pairs e.g *(:a 1)*,
assign them to *key* and *value*,
and finally set *key* to *value* on the hash table.

Now we can use this syntax to create new hash tables:
```
* (dict :a 1
        :b 2)
#<HASH-TABLE :TEST EQUAL :COUNT 2 {1002873B63}>
```

And since all we did was abstract the syntax,
we can still use regular functions that act on hash maps with it:
```
* (gethash :name
           (dict :name "felipe"
                 :email "email@example.com"))
"felipe"
T
```

## Extra Parenthesis Around *let*

When using *let* you probably noticed that each binding needs an extra pair of parenthesis,
we can avoid that by converting a property list to the nested list *let* expects.

So our end goal would be to type something like this:
```
(let (x 10
      y 20)
  (* x y))
```
Instead of:
```
(let ((x 10)
      (y 20))
  (* x y))
```

Let's start by converting a property list to a nested one:
```
(defun nest-plist (plist &optional (acc '()))
  "Returns a nest version of PLIST.

Example:

  (nest-plist '(x 5 y 10))
  ; =>
  ((x 5) (y 10))

"
  (cond ((null plist) (nreverse acc))
        (t (nest-plist
             (cddr plist)
             (cons (list (car plist) (cadr plist))
                   acc)))))
```

This function will loop through our property list and create new lists containing the *key* and *value*,
pretty similar to what we did in the hash table macro.

The macro itself is pretty simple:
```
(defmacro letn (bindings &body body)
  `(let ,(nest-plist bindings)
     ,@body))
```

Before passing *bindings* to *let* we first pass them to *nest-plist*,
and then we just splice *body* inside *let*.

## Conclusion

The magic thing about macros is that, in the end of the day, they're just code.
No matter how crazy and complicated one might be,
they will become regular Lisp code you could've written yourself.

Like functions, that let we abstract funcionality,
macros let us abstract syntax.
This allows us to map our programs to the problem domain like no other tool.
