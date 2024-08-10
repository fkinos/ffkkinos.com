+++
date = "2024-08-10T00:00:00-03:00"
title = "Lisp and Simbolically Computation"
+++

Programming in Lisp is like having a conversation with the computer, developing a close relantionship with the interpreter is very important.

## Conversation Starters

One of the easiest ways to start talking to your Lisp interpreter is to send it simple numbers, if you send it a number:
```
10
```

it will responde with its value:
```
10
```

## Combinations

To spicy up our conversation why don't we ask Lisp to compute operations on numbers as well? We can ask our Lisp interpreter to compute addition, subtraction, multiplication, and division.

```
(+ 1 2)
3
```

We start with parentheses, followed by the operation we want to perform and its operands. Expressions like this are called *combinations*.

```
(* 4 5)
20

(- 10 2)
8

(/ 36 9)
4
```

## Giving Values Names

Lets say we got Lisp to compute the value of some combination but it's kind of cumbersome to repeat the same combination over and over when we want to talk about this specific value. Luckily we can ask Lisp to give this expression a name, that will make it remember the value and allows us to refer to it by name!

```
(define my-awesome-value (/ (+ (- 3) 6) 2))
my-awesome-value
3/2
```

We start with the *keyword* *define* followed by the name we want to give this expression and the expression itself.

## Procedures

Procedures are like templates for computations we want to perform often with varying inputs.

We use the special *keyword* *lambda* to create procedures, next to it is our parameters delimited by parentheses and then the *body* of our procedure:

```
(lambda (x) (* x x))
```

In this example, we create some procedure with a parameter *x* and its body is a combination multiplying *x* by itself.

The procedure by itself doesn't do much but as we just learned, we can give any names to expressions to refer to them by name later. Let's do this with lambda as well:

```
(define square
  (lambda (x)
    (* x x)))

(square 5)
25
```

Here we call this procedure *square*, to apply it to some input we create a combination just like with the primitive procedures like *+* or *\**.

Lisp tells us that the *square* of *5* is *25*, how nice of it!

### Shorthand

We often create various procedures while talking to Lisp, so much so that a shorthand way of defining procedures was invented.

```
(define (square x)
  (* x x))
```

We start with *define* like before but now we wrap both the procedure name and its *parameters* in parentheses, and then the *body* of our procedure.

It just makes it extra nice to define more procedures if needed.
