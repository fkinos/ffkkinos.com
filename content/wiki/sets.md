+++
date = "2024-07-20T00:00:00-03:00"
title = "Naive Set Theory"
+++

A sets is a collection of mathematical objects. The order doesn't matter and we ignore duplicates.

We write sets as a comma-separated list surrounded by curly braces.

Here's some set A = { 1, 2, 3 } containing the objects 1, 2, and 3.

Here's another set B = { a, b, b, c, a } containing the objects a, b, c.

Here's another set C = {} containing no objects. It's empty.

When naming our sets, by convention, we usually start with the letters A, B, C, and so on.

## Elements

To say that some object x is an element of A is to say that x can be found in the collection A.

We denote it by x is an element of A or x is in A or x&in;A.

Alternatively, we could say that x is *not* an element of A or x&notin;A.

## Operations on Sets

Consider two sets, A = { 1, 2, 3} and B = { 3, 4, 5 }.

We can perform operations on them, each operation yielding a new set.

### Union

The union operator creates a new set containing objects that are either on A or on B.

So this new set would have the objects 1, 2, 3, 4, and 5 as its elements.

We denote it by A *union* B or A&cup;B = { 1, 2, 3, 4, 5 }.

### Intersection

The intersection operator creates a new set containing object common to both A and B.

So this new set would only have the object 3 as its element since it's the only object in both sets.

We denote it by A *intersection* B or A&cap;B = { 3 }.

### Difference

The difference operator creates a new set with all elements of A but removing any elements that are also in B.

So this new set would have the objects 1, and 2. Removing 3 since it's also an element of B.

We denote it by A *difference* B or A&setminus;B = { 1, 2 }.

## Set-Builder and Set-Roster Notation

All the sets we've seen so far are written listing out all its elements, that's called set-roster notation, set-roaster notation is pretty straight-forward, you just list all elements contained in said set, they're very easy to read and write. See for example:

The set containing all positive even integers:

A = { 2, 4, 6, 8, 10, … }. The three dots at the end are used to indicate that this pattern continues forever.

The set containing this sequence:

B = { 1, 20, 110, 340, 780, 1500, 2570, …, 500500, 544620 }. The three dots in the middle are used to indacate that the pattern continues up to the last elements listed.

But it doesn't take long to realize the downsides of this notation, the pattern it presents must be obvious to anyone reading it, otherwise we cannot possibly know what's the next element going to be.

For constructing larger and/or more precise sets we can use the set-builder notation. Instead of listing all elements of a particular set, we write a condition that every object must meet in order to be included in the set. See for example:

The set containing all positive even integers:

A = {x | x is an positive even integer } or A = {x&in;&naturals; | 2x and x > 0 }. Here are two always to write the same set in set-builder notation. We begin by specifying an variable and a condition said variable must meet.

## Family of Sets

Sets are a collection of mathematical objects and sets themselves are mathematical objects so we can have sets containing other sets as its elements with no problems.

### Power Sets

## Famous Sets
