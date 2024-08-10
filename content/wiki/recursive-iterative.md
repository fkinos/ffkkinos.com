+++
date = "2024-08-10T00:00:00-03:00"
title = "Recursive And Iterative Algorithms on Recursive Functions"
+++

Recursive functions get their name from the fact that they're defined on terms of themselves, that is,
they call themselves inside their definition.

But even though some function might be recursive the algorithm it implements by be effectively iterative.

Let's see how this work while implementing factorial in code:

```
(define (fact n)
  (if (<= n 1)
      1
      (* n (fact (- n 1)))))
```

Here we check if *n* is less than or equal to 1, if so we return 1, otherwise we return *n* times the result of calling *fact* with *n* minus 1.

Let's see what shape each call to *fact* produces:

```
(fact 4)
(* 4 (fact 3))
(* 4 (* 3 (fact 2)))
(* 4 (* 3 (* 2 (fact 1))))
(* 4 (* 3 (* 2 1))
(* 4 (* 3 2))
(* 4 6)
24
```

On each iteration we defer some computation to do afterwards, we connot compute them without first evaluating what the next value of *fact* is.

Let's contrast it with this other implementation of factorial:

```
(define (fact acc n)
  (if (<= n 1)
      acc
      (fact (* acc n) (- n 1))))
```

Here we check if *n* is less than or equal to 1, if so we return *acc* (standing for accumulator), otherwise we call *fact* again passing in *acc* times *n* and *n* minus 1.

Let's see what shape each call to *fact* this implementation produces:

```
(fact 1 4)
(fact 4 3)
(fact 12 2)
(fact 24 1)
24
```

We are no longer deferring some computation to do afterwards, we only need the arguments passed to *fact* in order to compute the next value.

So even though both functions are recursive in nature, that is, they call themselves in their definitions, they produce very different shapes.

## Tail Position

On our second implementation calls itself the *tail position*, that is, as the last action that procedure will execute. We no longer need to *remember* to do those deferred computations, all we need is available through the procedure's parameters.

Our first implementation, in the other hand, doesn't call itself as the last action, it's called inside a multiplication operation. We cannot possibly compute the value of the multiplication operation without evaluating subsequent calls to *fact*, thus we build this chain of deffered computations.

## Space and Time

Our first implementation is said to be linear in time, the time it takes is linear to its argument *n*. It's also linear in space, it *grows* more depending on the value of *n*.

The second implementation is also linear in time but it's constant on space, it doesn't need to *grow* as much.
