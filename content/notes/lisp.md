+++
date = "2024-03-09T00:00:00-03:00"
title = "Lisp"
+++

Lisp stands for **Lis**t **P**rocessing, and lists are what makes it so powerful yet simple and elegant.

Pairs
-----

Lists are not atomic, they're made up of smaller connected parts known as pairs, pairs have two components known as first and rest. Both components can be any Lisp atom like an integer, a string, etc, including other pairs, each with their own first and rest component.

Pairs are denoted by a parenthesized list separated by a dot, also known as dotted notation. Here's an example showing a pair with its first component being the integer 1 and its rest component being the symbol _lemon_:

```
'(1 . lemon)
```

Here is another pair with its first component being the integer 1, just like we saw in the previous example, but this time its rest component is another pair with first and rest components being the integers 2 and 3, respectively.

```
'(1 . (2 . 3))
```

The same dotted pair might also be represented as:

```
'(1 2 . 3)
```

With only the innermost pair presenting a dot.

Lists
-----

But Lisp is all about lists, right? where are they?? Calm down, we're getting there!

Lists are just pairs with their innermost pair having their rest component as _nil_.

```
'(1 . (2 . (3 . nil)))
```

Lists are represented without all the dots and parenthesis:

```
'(1 2 3)
```

You can think of _nil_ as something similar to C's NULL, a value that represents nothign in the language.

Data
---------

You might have noticed that simple quote character in the beggining of the previous Lisp expressions. By quoting some value, be it an atom or a list or pair, we are transforming whatever that value was to a symbol. Symbols are usually represented with strings, but unlike them, symbol are atomic, you cannot get its first caracter like you would with a string, for example. Let's take a look at what _quote_ does to some expressions:

```
* 'lemon
lemon
* '1
1
* '(1 . 2)
(1 . 2)
* '(1 . (2 . (3 . nil)))
(1 2 3)
```

Quoting something transforms that thing in into data, it's not treated as code but we can still manipulate it and even evaluate _quoted_ expressions to get their value back.

```
(1 2 3)
```

If you input something like the expression above (without a quote) you will get an error telling you that the integer 1 is not a function. You certainly know that the integer 1 is not a function already so why is Lisp telling you that?

Functions
---------

Lists are a bit special remember? so Lisp uses lists for function calls as well, the first list item is the function and the other items are treated as arguments to the function.

```
(+ 1 2 3)
```

In the expression above we have the function _+_ with arguments 1, 2, and 3. As you might have guessed, this is how addition works on Lisp, as oppossed to `1 + 3` on other languages.

If we call functions with lists it would make sense to create them using lists as well and that is exacly how it works!

That's where _lambdas_ come in, the first element of the list needs to be the symbol _lambda_ followed by a list with the lambda's parameters, and ending with the lambda's body.

```
(lambda (x) (* x x))
```

The example above is defining a function that takes in an variable named _x_ and returns _x_ squared. Here making use of the _*_ function which works the same as _+_ but for multiplying numbers instead of adding them.

```
((lambda (x) (* x x)) 5)
```

We can call lambdas like the built-in functions _+_ and _*_ by making it the first item of a list and providing some input with the other list items.

Variables
-------

We can define variables, not surprisingly, with lists.

By starting a list with the symbol _define_ we can bound symbol to values and refer to them throughout our programs.

```
(define x 5)
```

The second list item is the symbol we want to use to refer to the value at the third position.

Example
-------

Let's put some of these concepts to test:

```
(define fact
  (lambda (n)
    (if (zerop n)
        1
        (* n (fact (- n 1))))))
```

Here we are defining a function, which we will call _fact_, that will calculate the factorial of a number. There's a few new concepts which are worth closer inspection.

* We can store function like any other values making it the third list item on a list starting with _define_.
* If statements are lists as well (surprising, I know), the first argument in the list is the symbol _if_, the second is the condition, the third is an expression that will be evaluated when the condition is true and the fourth item is an expression that will be evaluated when the condition is false.
* _zerop_ is a function that expects one argument and returns true if that argument is zero, the suffix _p_ is used to indicate that it's a predicate and will return a boolean value.
* In the else branch of the if statement we are calling the same function we are currently defining, this is making a recursive call to _fact_ but using different arguments, making it converge to the base case (_n_ equals 0) in each call, otherwise we would end up in a infinite loop.
