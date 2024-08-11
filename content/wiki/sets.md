+++
date = "2024-07-20T00:00:00-03:00"
title = "Naive Set Theory"
+++

A sets is a collection of mathematical objects. The order doesn't matter and we ignore duplicates.

<!-- Here's another set C = {} containing no objects. It's empty. There's only one empty set and it's denoted by the symbol &empty;. -->

<!-- When naming our sets, by convention, we usually start with the letters A, B, C, and so on. -->

## Elements

Some object x is an element of A if x is part of the collection A.

We say x *is an element of* A or x *is in* A or x&in;A.

Alternatively, we could say that x is *not* an element of A or x&notin;A.

## Operations on Sets

Let A = { 1, 2, 3} and B = { 3, 4, 5 }. We can perform operations of two or more sets, each operation yields a new set.

### Union

The union operator creates a new set containing all objects that are both in A or in B.

So the *union* of A and B or A *union* B or A&cup;B will contain the object 1, 2, 3, 4, and 5.

A&cup;B = { 1, 2, 3, 4, 5 }

### Intersection

The intersection operator creates a new set containing object common to both A and B.

So the *intersection* of A and B or A *intersection* B or A&cap;B will contain the object 3 since it's the only object in both sets.

A&cap;B = { 3 }

### Difference

The difference operator creates a new set with all elements of A but none of the elements of B.

So the *difference* of A and B or A *difference* B or A&setminus;B will contain the object 1, and 2.
Not including 3 since it's an element of B.

A&setminus;B = { 1, 2 }

## Subsets

We say that A *is a subset of* B if all elements of A are also elements of B.

Let A = { 1, 2 } and B = { 1, 2, 3, 4, 5 }. Here A *is a subset of* B or A&sub;B.

## Set-Builder and Set-Roster Notation

All the sets we've seen so far are discribed by listing out all its elements,
that's called set-roster notation, set-roaster notation is pretty straight-forward,
they're very easy to read and write. See for example:

The set containing all positive even integers:

A = { 2, 4, 6, 8, 10, … }. The three dots at the end are used to indicate that this pattern continues forever.

The set containing this sequence:

B = { 1, 20, 110, 340, 780, 1500, 2570, …, 500500, 544620 }. The three dots in the middle are used to indacate that the pattern continues up to the last elements listed.

But it doesn't take long to realize the downsides of this notation, the pattern it presents must be obvious to anyone reading it, otherwise we cannot possibly know what's the next element going to be. And it's just tiring to write out all elements of some big set.

For constructing larger and/or more precise sets we can use the set-builder notation. Instead of listing all elements of a particular set, we write a condition that every object must meet in order to be included in the set. See for example:

The set containing all positive even integers:

A = {x | x is an positive even integer } or A = {x&in;&naturals; | 2x and x > 0 }. Here are two always to write the same set in set-builder notation. We begin by specifying an variable and a condition said variable must meet.

## Family of Sets

Sets are a collection of mathematical objects and sets themselves are mathematical objects so we can have sets containing other sets as its elements with no problems.

Let A = { {1, 2}, {3, 4}, {5, 6} }. A only has 3 elements, the set { 1, 2 }, {3, 4}, and { 5, 6 }. The objects 1 or 2 are not elements of A.

### Power Sets

The power set of a set contains all possible subsets of that set. Let A = { 1, 2 }, then *the power set of* A or, as it's usually denoted, *P*(A) = { &empty;, {1}, {2}, {1, 2} }.

The empty set is a subset of all sets.

## Famous Sets
