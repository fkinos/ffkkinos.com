+++
date = "2024-03-25T00:00:00-03:00"
title = "Introduction to Lisp"
toc = true
+++

Lisp is a strange language.
It revolves around the idea of using lists as the language's most basic construct,
lists perform the role of variable declaration, loops, and even functions.

Throughout this note whenever I say Lisp I'm referring to Common Lisp.
The _*_ character is used to indicate input in the REPL[^1].

## Basics

Let's start with a small function definition and function call to explain some key concepts:
```
(defun square (number)
  (* number number))
```

Lists are defined by parenthesis, anything between them are lists.
The first list, the one that starts with the _defun_ symbol[^2] declares a function,
it follows a specific structure.
```
(defun _name_ (param1 param2 ... paramN)
  value1
  value2
  ...
  valueN)
```

The first thing after _defun_ is the function's name,
next is a list of all parameters the function takes,
then is the function's body which may take any number of expressions.

The last value in the body will be returned as the function value.

To call function we also use lists.
The first item in the list is the function name we want to call,
all items after that are arguments to said function.
```
* (+ 1 2 3)
6
```

In this list above, the function name is _+_, and as you might have guessed,
this is how we perform addition in Lisp,
we then pass the numbers _1_, _2_, and _3_ to it, it returns _1_ + _2_ + _3_, which is _6_.

To call our on functions we do the same,
a list with the function name, followed by its arguments.
```
* (square 6)
36
```

The square of _6_ is _36_ and it gives us exactly that.

## Variables

We can define global variables using the following functions:
```
* (defparameter *name* "felipe")
*NAME*
* (defvar *likes* "soda")
*LIKES*
```

We can retrive their values by simply referring to them:
```
* *NAME*
"felipe"
* *LIKES*
"soda"
```

*defparameter* will always assign a value to whatever symbol we give it,
*defvar* does it only once.

This might seem odd but they're usecases for both of them.

By convention, we define global variables, like the ones above,
to have this an star in the start and in the end.
It doesn't have any special meaning, it just makes it easier for other developer (including your future self).

## Quote

Quoting is the act of tranforming code into data.
For example, when calling the _+_ function to sum numbers,
we create a list with the _+_ symbol, followed by its arguments,
and it returns the sum of those arguments, like expected.

But if we were to quote it, it would return the whole expression as data,
not evaluating it like in previous examples.
```
* (+ 1 2 3)
6
* (quote (+ 1 2 3))
(+ 1 2 3)
```

Quoting is so common that there's a special syntax for it,
we can replace the _quote_ call with a simple quote _'_ before the expression we want to quote.

```
* '(+ 1 2 3)
(+ 1 2 3)
```

## Built-in Functions

Some basic functions that we will all the time are _+_, _-_, _*_, and _/_,
we already encountered _+_ to perform basic addition,
the other functions work pretty much the same way.

Subtraction:
```
* (- 10 5 1)
4
```

Multiplication:
```
* (* 5 5 5)
125
```

Division:
```
* (/ 50 10 5)
1
```

Less than and Greater than:
```
* (> 8 5)
T
* (< 5 1)
NIL
```

Lisp represents true and false with the symbols _T_ and _NIL_,
Anything that's not _NIL_ or an empty list _'()_ is considered true.

Lisp also provides functions to deal with lists,
things like getting the first item in the list,
or applying some other function to all list items, etc.
```
* (first '(1 2 3))
1
* (rest '(1 2 3))
(2 3)
```

_first_ will get the first item in our list,
and _rest_ will get the remaining items.
To get the second item we can compose these functions together:
```
* (first (rest '(1 2 3)))
2
```

[^2]: Symbols are usually represented by a string of characters,
something like _defun_ or _function-name_,
but unlike strings, symbols are atomic,
you can't manipulate a symbol like you would with a string.
Symbols can be used to all sort of things, usually are used for identifying something like a function or variable.

[^1]: REPL stands for Read-Eval-Print-Loop, if you ever started python through the commandline, that's a REPL!
