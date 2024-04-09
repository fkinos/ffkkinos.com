+++
date = "2024-03-25T00:00:00-03:00"
title = "Introduction to Lisp"
toc = true
+++

Lisp is a strange language. It revolves around the idea of using lists as the
language's most basic construct, lists perform the role of
variable declaration, loops, and even functions.

Thoughout this note whenever I say Lisp I'm referring to Common Lisp. The _*_ character is used to indicate input in the REPL[^1].

## Basics
Let's start with a small function definition and function call to explain some key concepts:

```
* (defun square (number)
    (* number number))
SQUARE
* (square 5)
25
```

Lists are defined by parenthesis, anything between them are lists.
The first list, the one that starts with the _defun_ symbol[^2]
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

## Variables
As we saw earlier, using a list starting with _defun_ allows us to define a symbol that is associated with a function, we have similar lists for storing variables.

```
* (defparameter *name* "john")
*NAME*
* *name*
john
```

The simplest one is _defparameter_, it will always set the value of the symbol you passed as the second item to the value you passed as the third item.

By convention, we define global variables, like the one above, to have the _*_ prefix and suffix. It doesn't have any special meaning, it just makes it easier for developers.

## Quote
Quoting is the act of tranforming code into data.
For example, when calling the _+_ function to sum numbers,
we create a list with the _+_ symbol, followed by its arguments,
and it returns the sum of those arguments, like expected.

But if we were to quote it, it would return the whole expression as data, not tranforming it into code, like the previous examples did.

```
* (+ 1 2 3)
6
* (quote (+ 1 2 3))
(+ 1 2 3)
```

Quoting is so common that there's a special syntax for it, we can replace the _quote_ _macro_ with a simple quote _'_.

```
* '(+ 1 2 3)
(+ 1 2 3)
```

## Built-in Functions
Some basic functions that we will all the time are _+_, _-_, _*_, and _/_,
we already encountered _+_ to performe basic addition,
the other functions work pretty much the same.

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
Lisp represents true and false by the symbols _T_ and _NIL_,
the first function is asking _is 8 greater than 5_, and the second function is asking _is 5 less than 1_.

Lisp also provides functions to deal with lists, things like getting the first item in the list, or applying some other function to all list items, etc.
```
* (first '(1 2 3))
1
* (rest '(1 2 3))
(2 3)
```

_first_ will get the first item in our list,
and _rest_ will get the remaining items.
To get the second item we can combine these functions to do what we want:
```
* (first (rest '(1 2 3)))
2
```
Here we start to understand how lists in Lisp actually work. They're made up of cells that contain any Lisp value, like a number, string, etc.


_mapcar_ will apply a function to all elements of a list we pass to it.
```
* (mapcar (lambda (n)
            (* n n))
          '(1 2 3))
(1 4 9)
```

Here instead of some function we defined earlier with _defun_,
we are creating an anonymous function. It's pretty similar to _defun_ but instead of the symbol _defun_ and the function name, we just use the symbol _lambda_.

And, as we saw in the previous section, we are _quoting_ the list we're passing to _mapcar_ to avoid evaluating it, as it would cause an error since there isn't a function called _1_.

```
* (apply #'+ '(1 2 3))
6
```

_apply_ expects a function as its first parameter, and a list as its second parameter.
To get a function instead of just the symbol _+_, we use this _#'_ prefix.
And we're quoting the list to avoid evaluating it, like we did previously.

## Macros
Macros allows us to generate code on the fly, making use of _quotation_ and _quasi-quotation_.
Let's create a macro to make the greater than and less than functions a bit more familiar to use.

```
* (defmacro compare (a op b)
    `(,op ,a ,b))
COMPARE
* (compare 5 > 10)
NIL
* (compare 8 < 12)
T
```

At first, _defmacro_ seems pretty similar to _defun_,
we can name it and declare a list of parameters it expects.
But its body is a lot more interesting.

The backtick character is called _backquote_,
like _quote_ it interprets things in it as data,
but it allows us to evalute what we want using the _comma_ operator.

So the macro above is saying, _please create a list of symbols but evaluate the symbols op, a, and b_.

[^2]: Symbols are usually represented by a string of characters,
something like _defun_ or _function-name_,
but unlike strings, symbols are atomic,
you can't manipulate a symbol like you would with a string.
Symbols can be used to all sort of things, usually are used for identifying something like a function or variable.

[^1]: REPL stands for Read-Eval-Print-Loop, if you ever started python through the commandline, that's a REPL!
