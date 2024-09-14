+++
date = "2024-08-10T00:00:00-03:00"
title = "Recursive And Iterative Algorithms on Recursive Functions"
+++

Recursive functions get their name from the fact that they're defined on terms of themselves, that is,
they call themselves inside their own definitions.

But even though some function might be recursive the algorithm it implements might be effectively iterative, meaning that it's as efficient as ordinary loops while having a more understandable recursive definition!

Let's see how this work while implementing factorial in code, here will be using [Lisp](/wiki/lisp) but the concept should apply to pretty much all languages.

## Factorial Example

We will define two functions that compute the factorial of some number, both of them will be recursive, in the sense that they call themselves, but one of them will implement a recursive algorithm and the other one will implement a iterative algorithm.

Let's take a look at the recursive algorithm first!
```
(define (fact n)
  (if (<= n 1)
      1
      (* n (fact (- n 1)))))
```

It's pretty straight-forward, we check if `n` is less than or equal to 1, if so we return 1, otherwise we return `n` times the result of `fact` with one less than `n`.

Now we will walk through each call to `fact` to see how it behaves and expands.

The first it's called it just receives whatever number we passed to it, in this example, `n` equals 4.
```
(fact 4)
```

Since 4 is greater than 1 it will skip the base case and expand with a call to `fact` with one less than `n`, as per the definition.
```
(* 4 (fact (- 4 1)))
```
Evaluating the arguments to `fact` we get:
```
(* 4 (fact 3))
```

This pattern continues until we reach the base case of `n` being less than or equal to 1:
```
(* 4 (fact 3))
(* 4 (* 3 (fact 2)))
(* 4 (* 3 (* 2 (fact 1))))
(* 4 (* 3 (* 2 1))
(* 4 (* 3 2))
(* 4 6)
24
```

The computation *grows*, producing this sort of triangular shape of deferred computations we can't still answer, up until the last recursive call.

Only after evaluating each recursive call we were able to perform the calculation, it wouldn't be able to do the calculation without first calculating what the next value of `fact` is.

Let's contrast it with the other implementation of factorial, using a iterative algorithm:
```
(define (fact acc n)
  (if (<= n 1)
      acc
      (fact (* acc n) (- n 1))))
```

It's a bit more complicated than the last one, we are building up the result of `(* acc n)` in one of the function's parameters, while decreasing `n` with each call. When we reach the base case we simply return that parameter.

Let's see what pattern the calls of this version of factorial produces:
```
(fact 1 4)
(fact 4 3)
(fact 12 2)
(fact 24 1)
24
```

Just by looking at the *shape* of this algorithm we can already see a pretty big difference.

We no longer defer computations, we can compute everything by only using the parameters that were passed in.

So even though both functions are recursive in nature, that is, they call themselves in their definitions, they produce very different shapes.
