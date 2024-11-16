+++
date = "2024-07-24T00:00:00-03:00"
title = "Proving Mathematical Statements"
+++

Throughout most of our mathematical carrers while studentents we don't necessarily know why or how mathematics works, it just does.

Let's deepen our understanding of mathematics by learning to write and read proofs, mathematical arguments used to convince other mathematicians that some conclusion is true by the use of [logic](/wiki/logic) and [set theory](/wiki/sets).

## Writing *Good* Proofs

Proofs are essays, it's very important to clearly express what you're doing,
what method you're using, don't rely on simbolisms, introduce notation carefully,
and don't be condensending to your reader.

## Scratch Work

It's pretty difficult to jump into proofing right away after first encountering some theorem.
We usually take our time to understand what the theorem requires from us,
we sketch out some possibilities, remind ourselves of the relevant definitions, and so on.

This is usually called *scratch work*.

## Tradition

After we've proved some theorem let's mark it with an X, think of it as marking this theorem as done on a imaginary theorems checklist containing all theorems we will prove someday!

Everyone likes to end their proofs in some particular way,
some end it with [QED](https://en.wikipedia.org/wiki/Q.E.D.),
other with a little square, others with a filled square, and so on.

Just make sure you signal to your reader when your proof ends.

## Proof Techniques

- Direct Proof

> In this kind of proof we assume the premise to prove the conclusion.

- Proof by Contrapositive

> In this kind of proof we assume the opposite of the premise to prove the opposite of the conclusion.

- Proof by Contradiction

> In this kind of proof we assume the premise is false and try to get to a contradiction.

- Proof by Induction

> **_TODO_**

### Direct Proof

When encountering some goal of the form P implies Q or If P, then Q, we can assume P and use it to prove Q.

**Theorem**. Let a and b be integers. If a and b are even, then a+b is even.

*Scratch Work*

The theorem requires us to know what makes some number even and if that property holds when adding two numbers.

Let's remind ourselves of what being an even number means:

An even number is some number n such that n=2k for some integer k. In other words, a number is even if it can be represented by some other number multiplied by 2.

Looking back at the theorem we can see that it tells us that both a and b are integers, that's great because evens or odds are only possible with integers.

We then see this "If, then" statement, from it we can gather our premises and conclusions.

Everything after the "If" and before the "then" is our premise, everything after "then" is our conclusion.

Using direct proof we can assume our premise is true to try to prove our conclusion.

So let's assume that a and b are even, that means that both can be represented as a=2m for some integer m and b=2n for some integer n.

Substituting this new representation in the original conclusion we get a+b=2m+2n=2(m+n). If we make k=m+n, then a+b=2k. And that's exatcly the definition for an even number.

Let's prove it!

**Proof**. Suppose a and b are even. That means a=2m for some integer m, and b=2n for some integer n. So a+b=2m+2n=2(m+n). Thus a+b=2k where k=m+n and k is an integer. Therefore, if a and b are even, then a+b is even. X

The proofs themselves are way more concise and don't include a lot of the details,
that's why it's important to learn to write them ourselves so we can understand proofs written by others.

### Proof By Contrapositive

When proving some goal of the form not Q implies not P or If not Q, then not P, we can assume not Q and use it to prove not P.

Note: We can also use this technique on some goal of the form P implies Q since P implies Q and not Q implies not P are logically equivalent.

**Theorem**. Let a, b, and c be real numbers and a > b. If ac<=bc, then c<=0.

*Scratch Work*

Since we're proving this theorem using the contrapositive we are going to assume not Q to prove not P.

In this case Q is c<=0 so not Q is c>0, and P is ac<=bc so not P is ac>bc.
Thus, if c>0, then ac>bc.
But look, this has the form P implies Q, so we can assume P and use it to prove Q!

So let's assume c>0.

Looking at the theorem again, it tells us that a>b.
Multiplying both sides by c gets us ac>bc.

**Proof**. We will prove the contrapositive. Suppose c>0. Multiplying both sides of the given inequality a>b by c we can conclude that ac>bc. Therefore, if ac<=bc, then c<=0. X

### Proof By Contradiction

When proving some theorem by contradiction we assume P is false and try to get to a contradiction. If we get to a contradiction using valid logic rules and inferences that means that our initial hypothesis was false. Therefore, if we get to a contradiction assuming P is false then P must be true.

**Theorem**. The empty set is subset of every set.

*Scratch Work*

The empty set is defined to be the set with no elements.

We say that A is a subset of B if every element of A is also an element of B.

Using these definitions we can conclude that every element in the empty set must be also in some set A.
We can assume the opposite, that the empty set is not a subset of A.
This means that there must exist some element of the empty set that is not in A.
But here we contradict the very own definition of the empty set.
Thus, we've reached a contradiction!

**Proof**. Suppose &empty; is not a subset of A. This means that there exists some x in &empty; such that x is not in A. Note x in &empty; is absurd, a contradiction. Thus, the empty set is a subset of every set. X

### Proof By Induction

**_TODO_**
