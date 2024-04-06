+++
date = "2024-04-03T00:00:00-03:00"
title = "Introduction to Lisp"
toc = true
+++

Lisp is a strange language. It revolves around the idea of using lists as the
language's most basic construct, lists perform the role of
variable declaration, loops, and even functions.

Thoughout this note whenever I say Lisp I'm referring to Common Lisp.

## Basics
Let's undertand some building blocks of the language before
jumping into a full-on example program.

### Types
You can create of sort of things in Lisp, integers, floating point numbers,
fractions, strings, characters, etc.

Let's start up a REPL[^1] to test some expressions,
the _*_ character indicates our prompt:
```
* 1
1
* 0.5
0.5
* 10/3
10/3
```

As you probably expected, the integer _1_ evaluates to _1_,
this is also true for any other integer and floating point number, fortunately.

Lisp also allows us to use fractions without losing precision, not converting them to a floating point number automatically.

```
* "hello, world!"
"hello, world!"
* #\a
#\a
```

Strings are pretty straight-forward, anything between double quotes is a string.
Characters are a bit different from other languages but are not surprising as well.

### Pairs and Lists
Pairs are a data structure that let's join things together, pairs have two
components, the first and the rest. Let's look at some examples.

```
* '(1 . "one")
(1 . "one")
```

In the expression above, the first component of this pair is the integer _1_ and the rest component is the string _"one"_. We can have any Lisp expression as the first or the rest component, including other pairs:

```
* '(1 . (2 . 3))
(1 2 . 3)
```

If we the inner-most pair's rest component is _nil_ we form a list!

```
* '(1 . (2 . (3 . nil)))
(1 2 3)
```

We can create the same list in a much nicer way:
```
* '(1 2 3)
(1 2 3)
```

You might have noticed that simple quote in the beginning of the expressions when dealing with pairs and lists, we'll address this shortly.

## Functions
The reason we needed that simple quote in the last examples is because,
by default, Lisp will interpret anything other than the basic types as a function call.

If we remove the simple quote from the list _'(1 2 3)_ Lisp would complain that _1_ is not a function.

Function calls have this structure, the first item in the list is the function
we're calling and the rest of the items are arguments to said function:

```
* (+ 1 2 3)
6
```

Here the function is the symbol _+_ and the arguments are _1_, _2_, and _3_.

[^1]: REPL stands for Read-Eval-Print-Loop, if you ever started python through the commandline, that's a REPL!
