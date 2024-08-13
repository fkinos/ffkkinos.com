+++
date = "2024-07-24T00:00:00-03:00"
title = "Proving Mathematical Statements"
+++

Throughout most of our mathematical carrers while studentents we don't necessarily know why or how mathematics works, it just does.

We can deepen our understanding of mathmatics by learning to write and read proofs, mathematical arguments used to convince other mathematicians that some conclusion is true by the use of logic.

To write and read proofs we need a good deal of natural language (english, portuguese, and so on), [Mathematical Logic](/wiki/logic), and a bit of [Set Theory](/wiki/sets).

## Writing *Good* Proofs

Proofs are essays, it's very important to clearly express what you're doing,
what method you're using, don't rely on simbolisms, introduce notation carefully,
and don't be condensending to your reader.

## Scratch Work

It's pretty difficult to jump into proofing some theorem right after first encountering it.
We usually take our time to understand what the theorem requires from us,
we sketch out some possibilities, remind ourselves of the relevant definitions, and so on.

This is usually called *scratch work*, in this page we will showcase all the *scratch work* involved on proving some theorem.

## Tradition

After we've proved some theorem let's mark it with an X, think of it as marking this theorem as done on a imaginary theorems checklist containing all theorems we will prove someday!

Everyone likes to end their proofs in some particular way,
some end it with [QED](https://en.wikipedia.org/wiki/Q.E.D.),
other with a little square, others with a filled square, and so on.

Just make sure you signal to your reader when your proof ends.

## Proof Techniques

### Direct Proof

When proving some goal of the form P→Q (P implies Q, or If P, then Q) we can assume P and use it to prove Q.

Let's see this in action with the following example.

**Theorem**. Let a and b be integers. If a and b are even, then a+b is even.

*Scratch Work*

It would be pretty hard to prove a number is even without even knowing what the definition of an even number is, so let's get this out of our way first:

An even number is some number x such that x equals 2k for some integer k. In other words, a number is even if it can be represented by some other number multiplied by 2.

And looking again at the theorem, it tells us that both a and b are even,
that means that both a and b can be represented by some number multiplied by 2.

Let's see if by substituting a and b by this other representation can help us somehow:

So a=2m for some integer m and b=2n for some integer n.
This means that a+b=2m+2n, factoring out a 2 we get a+b=2(m+n).

From the definition of an even number, m and n must be integers, thus their sum is also an integer.

Therefore a+b=2k where k=m+n,
and this is exatcly what it means for a number to be even,
a+b can also be represented by other number multiplied by 2,
in other words, a+b is even.

After figuring out all of this we are ready to prove this theorem!

**Proof**. Suppose a and b are even. That means a=2m for some integer m, and b=2n for some integer n. So a+b=2m+2n=2(m+n). Thus a+b=2k where k=m+n and k is an integer. Therefore, if a and b are even, then a+b is even. X

The proofs themselves are way more concise and don't include a lot of the details,
that's why it's important to learn to write them ourselves so we can understand proofs written by others.

### Proof By Contrapositive

When proving some goal of the form &not;Q&rarr;&not;P (not Q implies not P, or if not Q then not P) we can assume &not;Q and use it to prove &not;P.

We can also use this technique on some goal of the form P&rarr;Q since P&rarr;Q and &not;Q&rarr;&not;P are logically equivalent.

Let's see this in action with the following example.

**Theorem**. Let a, b, and c be real numbers and a > b. If ac&le;bc, then c&le;0.

*Scratch Work*

We will prove using the contrapositive,
which means that we are going to assume &not;Q to prove &not;P.

In this case Q is c&le;0 so &not;Q is c>0, and P is ac&le;bc so &not;P is ac>bc. Thus, if c>0, then ac>bc. But look, this has the form P&rarr;Q, so we can assume P and use it to prove Q! So let's assume c>0.

Looking at the theorem again, it tells us that a>b. Multiplying both sides by c gets us ac>bc, which is our new goal!

**Proof**. We will prove the contrapositive. Suppose c>0. Multiplying both sides of the given inequality a>b by c we can conclude that ac>bc. Therefore, if ac&le;bc, then c&le;0. X

### Proof By Contradiction

### Proof By Induction
