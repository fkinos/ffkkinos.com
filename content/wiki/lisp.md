+++
date = "2024-08-10T00:00:00-03:00"
title = "Lisp and Symbolic Computation"
+++

Programming in Lisp is like having a conversation with the computer, developing a close relantionship with the interpreter is very important.

Lisp is a very expressive language, and with all that expressiveness people started to do things in a way that made them more confortable. As a result of that, a bunch of different dialects of [Lisp](https://en.wikipedia.org/wiki/Lisp_(programming_language)) were born.

In this page I will be using a specific dialect of Lisp called  [Scheme](https://en.wikipedia.org/wiki/Scheme_(programming_language)).

Most things should work on most dialects but if something suddenly doesn't that's why!

## Starting Lisp

This will vary slightly depending on which dialect you're using, for the version of Scheme I'm using, called [Chez Scheme](https://cisco.github.io/ChezScheme/), I can start it by typing it `chez` into my terminal emulator.

Doing so will drop us right into the Lisp interpreter:
```
$ chez
Chez Scheme Version 10.0.0
Copyright 1984-2024 Cisco Systems, Inc.

>
```

Right now Lisp is waiting for us to start talking. We know that it's waiting by spotting the *>* character in the bottom of the screen. Our cursor will always be positioned there after each command.

We can talk about all sort of things, let's start by talking about some numbers!

## Numbers

In that same screen we got to earlier, let's hit the number 1 into the Lisp interpreter and then hit enter key:
```
$ chez
Chez Scheme Version 10.0.0
Copyright 1984-2024 Cisco Systems, Inc.

> 1
1
>
```

For Lisp 1 means 1, how exciting!

We can also talk about rational numbers, those pesky fractions, and real numbers, those we use to represent 'in-between' numbers like 1.5 or 10.1:
```
$ chez
Chez Scheme Version 10.0.0
Copyright 1984-2024 Cisco Systems, Inc.

> 1
1
> 1/2
1/2
> 3.14
3.14
>
```

These very basic commands like numbers are called atoms! The very foundations of our programs.

Personally I find numbers very exciting but I understand that might not be for everyone, let's learn more fun things to talk about!

### Note

For the sake of me writing a bit less I will not include all of the output of the Lisp interpreter in the examples, intead I will only include the current command we're performing.

So I will only show this little snippet:
```
> 1
1
>
```

But it will know about all the previous instructions, hopefully that's okay!

## Computations

To spicy up our conversation why don't we ask Lisp to compute operations on numbers as well? We can ask the Lisp interpreter to compute all operations a calculator would.

Let's start with addition:
```
> (+ 1 2 3)
6
>
```

To perform an operation we start by hitting the left parentheses character `(`, followed by the plus character `+` (for addition), and the numbers we want to add, all separated by a single space, then hit the right parentheses character `)`, and end by hitting the enter key.

We're not limited to adding just three numbers, Lisp let's us perform addition of as many numbers as we want:
```
> (+ 1 2 3 4 5 6 7 8 9 10)
55
>
```

To perform subtraction, multiplication, and division we perform the same steps but changing the character after the left parentheses `(` to the character related to the operation we want to perform.

Subtraction:
```
> (- 10 2 3)
5
>
```

Multiplication:
```
> (* 1 2 3 4 5)
120
>
```

And division:
```
> (/ 10 2)
5
>
```

## Complex Operations

Since the operations we just performed spit out a number, would it be possible to perform operations on them, just like we did with numbers?

And the answer is yes, we can!

We can mix and combine all the operations we've seen so far to perform more complex calculations. Let's take a look at one:
```
> (+ 1 (- 2 2))
1
>
```

Here we performed an addition operation but instead of a regular number we included another operation inside of it.

The Lisp interpreter first looked at all the things we've written after addition operator (the `+` character) and checked what their value were, as we've seen earlier, the value of numbers are themselves, and the value of `(- 2 2)` is another number, the number zero.

After figuring out what their values were, it simply added all of them together.

## Definitions

Let's say that after playing around with the Lisp interpreter we finally arrived at the calculation we wanted to perform.

Personally I wanted to know what the factorial of 5 was, to find the factorial of any number we simply multiple all numbers starting from 1 all the way up to the number we were interested in, so for the factorial of 5 the algorithm is to multiply 1, 2, 3, 4, and 5 together.

And as we just learned, the Lisp interpreter can perform multiplication just fine, so I asked it:
```
> (* 1 2 3 4 5)
120
>
```

And it answered me with the correct answer, 120!

But I wanted to use this value in other operations, it would be pretty inconvenient to include the same `(* 1 2 3 4 5)` operation everywhere, right?

Luckily we can ask the Lisp interpreter to remember values that are special to us, it calls them *definitions*!

We can create a new definition like this:
```
> (define factorial-of-five (* 1 2 3 4 5))
>
```

To add a new definition we start by hitting the left parentheses character `(`, followed by the *keyword* `define`, then the name we want it to remember for that specific value, in this case the name is `factorial-of-five`, and we write the operation we wanted it to remember, the multiplication operation from earlier, then hit the right parentheses character `)`, and end by hitting the enter key.

Please note that since both the multiplication operation and this new definition end with a right parentheses character `)`, the whole thing will have two right parentheses characters in the end.

It doesn't answer with anything but it surely remembers it pretty well!

We can ask for that value back by referring to it by the name we've giving, in this case `factorial-of-five`:
```
> factorial-of-five
120
>
```

How nice of the Lisp interpreter to remember things for us!

We can then use this name in new operations, just like we would with numbers or complex operations:
```
> (+ factorial-of-five 100)
220
>
```

## Functions

Functions in the mathematical sense are computations that given some input spit out some output, in a broader sense, it's just a way to reuse operations with different inputs (or no inputs at all). Let's see why they're usuful!

A quick way to check the square of some number is to use a multiplication operation with the same number twice:
```
> (* 5 5)
25
>
```

Multiplying 5 with 5 we get 25, the square of 5. And we can do the same with other numbers:
```
> (* 3 3)
9
> (* 4 4)
16
>
```

But can the Lisp interpreter remember algorithms for us? Similarly to how it could remember special values, it would be very nice for it to remember an algorithm like squaring a number but allowing us to provide different inputs.

And it surely can!

We can do it by creating a new definition, but with slightly more things:
```
> (define (square x) (* x x))
>
```

It's very similar to the last definition we did, we start with a left parentheses character `(`, followed by the *keyword* `define`, but now we add another left parentheses, followed by the name we want to give to this algorithm, in this case `square`, and the what inputs it will receive, in this case, a number which we will call `x`, followed by a right parentheses character `)`, now we write the same multiplication operation we did earlier when squaring five and four but with `x` instead of a specific number, then another right parentheses character `)`, and end by hitting the enter key.

Now we can perform this operation like we performed addition or multiplication:
```
> (square 5)
25
> (square 3)
9
> (square 4)
16
>
```

Now we don't need to worry about how to square a given number, we can just pass it to the `square` algorithm we just created!
