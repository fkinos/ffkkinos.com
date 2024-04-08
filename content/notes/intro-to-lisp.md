+++
date = "2024-03-25T00:00:00-03:00"
title = "Introduction to Lisp"
toc = true
draft = true
+++

Lisp is a strange language. It revolves around the idea of using lists as the
language's most basic construct, lists perform the role of
variable declaration, loops, and even functions.

Thoughout this note whenever I say Lisp I'm referring to Common Lisp. The _*_ character is used to indicate input in the REPL[^1].

## Basics
Let's start with a small function to explain some key concepts:

```
* (defun square (number)
    (* number number))
SQUARE
* (square 5)
25
```

Lists are defined by parenthesis, anything between them are lists.
The first list, the one that starts with the _defun_ [symbol](#symbols)
declares a function, it follows a specific structure.

```
(defun _name_ (param1 param2 ... paramN)
  value1
  value2
  ...
  valueN)
```

The first thing after _defun_ is the function's name,
next a list of all parameters the function takes,
then is the function's body which may take any number of expressions.
The last value in the body will be returned by the function.

To call function we also use lists.
The first item in the list is the name of the function we want to call,
all items after that are arguments to said function.

```
* (+ 1 2 3)
6
```

In the first list, the function name is _+_, as you might have guessed, this is how we perform addition in Lisp, we then pass the numbers _1_, _2_, and _3_ to it, it returns _1_ + _2_ + _3_, which is _6_.

To call our on functions we do the same,
a list with the function name, followed by its arguments.

```
* (square 6)
36
```

The square of _6_ is _36_ and it gives us exactly that.

## Symbols
Symbols are usually represented by a string of characters,
something like _defun_ or _function-name_,
but unlike strings, symbols are atomic,
you can't manipulate a symbol like you would with a string.
Symbols can be used to all sort of things, usually are used for identifying something like a function or variable.

[^1]: REPL stands for Read-Eval-Print-Loop, if you ever started python through the commandline, that's a REPL!
