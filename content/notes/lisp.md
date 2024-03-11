+++
date = "2024-03-09T00:00:00-03:00"
title = "Lisp"
description = "Some thoughts about Lisp and some cool little things I like about it."
+++

Lisp stands for **Lis**t **P**rocessing, and lists are what makes Lisp so powerful yet simple and elegant.

Pairs
-----

Lists are not atomic, they're made up of smaller connected parts known as pairs, pairs have two components, the first part and the rest. The first part is usually some other Lisp atom like an integer, string, etc and the rest is, well, the rest.

The rest is just another pair with a first component and a rest component, if you're familiar with data structures you'll probably recognize this as a linked list. Linked lists can perform badly but Lisp has tools to deal with that.

Pairs are denoted by a parenthesized list separated by a dot, also called dotted notation. Here's an example showing a pair with its first component being the integer 1 and its rest component being also the integer 1:

```
'(1 . 1)
```

Here's a pair with its first component being the integer 1 and its rest component being another pair with components 2 and 3:

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

Lisp is all about lists, right? but where are they?? Calm down, we're getting there!

Lists are just pairs with the innermost pair having their rest component be _nil_.

```
'(1 . (2 . (3 . nil)))
```

You can think of _nil_ a something similar to C's NULL.

Data
---------

You might have noticed the quote character in the beggining of the previous Lisp expressions. By having that quote there we create what is known as a _quoted_ expression, _quoted_ expressions are not code, they tell Lisp that whatever we are _quoting_ is data!

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
