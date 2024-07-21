+++
date = "2024-07-20T00:00:00-03:00"
title = "sets"
+++

![drawing show various set theory symbols](/set_theory.png)

sets are a mathematical object that allows us to group a bunch of other mathematical objects together,
they don't care what order those objects are inserted nor if they're any duplicates.

## notation

sets are usually written by starting with an open bracket '{',
a comma-separated list of objects, and ending with an closed bracket '}'.

example: { 1, 2, 3 }

you may read this as the set containing the numbers 1, 2, and 3.

## operations

we can apply a few operation on sets.

suppose A is the set { 1, 2, 3 } and B the set { 3, 4, 5 }.

UNION. we say that A&cup;B (A union B) is a new set where its elements are any elements of A or B. meaning that if x is an element of A or an element of B it should be an element of A&cup;B.

in this example A&cup;B would be the set { 1, 2, 3, 4, 5 }.

INTERSECTION. we can also say that A&cap;B (A intersection B) is a new set where its element are any elements of A and B. meaning that if x is an element of both A and B it should be an element of A&cap;B.

in this example A&cap;B would be the set { 3 }.

SUBSET. we can also say that A&sube;B (A is a subset of B). meaning that every element of A is also an element of B.

in this example the set A&cap;B is a subset of A and also a subset of B, since both sets contain all elements of A&cap;B, which in this example is just the number 3.

ELEMENT. we can say that x&isin;A (x is an element of A). meaning that an object x is contained by the set A.

in this example 2&isin;A and 4&isin;B.

NOT AN ELEMENT. we can also say that x&notin;A (x is not an element of A). meaning that an object x is not contained in the set A.

in this example 9&notin;A.

## constructing sets

we construct set in a few different ways.

suppose you want to create a set that lists all natural numbers from 10 to 19.

you could simply list them all like so.

{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19 }

that was easy but how should we write bigger sets, or infinite sets?

one way is to create a sequence that follows a understandable pattern, list a few elements, and end the list with three dots, implying it follows the established pattern.

{ 0, 1, 2, 3, …}

but what if the pattern was more complex? for that we could write a membership test!

let's rewrite the set of all natural numbers from 10 to 19.

{ x&isin;&Nopf; | 10 &le; x &le; 19 }

you may read this as "x belonging to the natural numbers such that x is greater than or equal to 10 and less than or equal to 19".

both ways of writing this set are equally valid.

## applications

sets are pretty handy to represent anything you want, all of mathematics is built upon the notion of sets anyway.

a pretty common use case is to represent a group of numbers like the natural numbers, or the real numbers. it can also be used to represent a specific universe for some [logic](/notes/logic) statement.