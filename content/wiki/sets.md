+++
date = "2024-07-20T00:00:00-03:00"
title = "Set Theory"
+++

A set is a collection of objects, any objects. The only rule is that it must not contain itself, otherwise it's not a set.

We represent sets with a comma-separated list surrounded by curly braces, just like so { 1, 2, 3 }.
We can also assign them to variables just like we already do with numbers or functions, like so A = { 1 }.

## Elements

We say that x is an element of some set A with x is part of the collection that A represents.

The set A = { 1, 2, 3 } has elements 1, 2, and 3. Or in other words, 1, 2, and 3 are elements of A.

If some element is not part of the collection that A represents we say that that element is not an element of A.

## Subsets

A subset is some set B that contains some (or all) elements of some set A.

If A = { 1, 2, 3 } and B = { 1, 2 }, B is a subset of A because all elements of B are also elements of A.

## Empty Set

The empty set or null set is simply a set with no elements. It's a subset of all sets.

The usual notation for it is a pair of curly braces with no elements in between like {} or the symbol &empty;.
Both mean the exact same thing.

## Set Operations

Set operations required two (or more) sets, by following each operation rule a new set is produced.

- Union
> The union of two sets A and B produces a new set with all elements that are in A or in B.

> Example:

> Let A = { 1, 2, 3 } and B = { 3, 4, 5 }, then A union B = { 1, 2, 3, 4, 5 }.

- Intersection
> The intersection of two sets A and B produces a new set with all elements that are in A and in B.

> Example:

> Let A = { 1, 2, 3 } and B = { 3, 4, 5 }, then A intersection B = { 3 }.

- Difference
> The difference of two sets A and B produces a new set with all elements of A that are not in B.

> Example:

> Let A = { 1, 2, 3 } and B = { 3, 4, 5 }, then A difference B = { 1, 2 }.

## Set-Builder and Set-Roster Notation

Set-Roster notation is used when simply listing all elements of a set like { 1, 2, 3 }.

Set-Builder notation is used when providing the instructions of how to find elements that belong to a set like { x | x > 1 }, meaning all objects x such that x is bigger than 1 belong to this set.

## Power Sets

The power set of some set A contain all possible subsets of said set.
If we let A = { 1, 2 } then the power set of A or P(A) = { &empty;, {1}, {2}, {1, 2} }.

## Family of Sets

As seem with power sets, sets can also contain other sets as their elements.

Let A = { { 1, 2 }, { 3, 4 }, { 5, 6 } }, A only has 3 elements, the sets { 1, 2 }, { 3, 4 }, and { 5, 6 }, these sets have 2 elements each.
