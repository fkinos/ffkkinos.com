+++
date = "2024-07-24T00:00:00-03:00"
title = "proofs"
+++

![drawing illustrating mathematical proofs](/proofs.png)

mathematical proofs allows us to be certain about mathematical statements, proving they're correct.

## direct proofs

we can directly prove a theorem with the form P&rarr;Q assuming P is true and using it to prove Q.

let's see this in action with the following example.

theorem: let a and b be integers. if a and b are even, then a+b is even.

proof. suppose a and b are even. this means a = 2k for some integer k and b = 2l for some integer l.
then a+b = 2k+2l, in other words a+b = 2(k+l), where k+l is an integer. therefore, if a and b are even, then a+b is even. QED

let's understand what went on building this proof. from the theorem and assuming P is true we get that a and b are integers and even numbers. recall that if x is even &harr; x=2k for some integer k. (similarly if x is odd &harr; x=2k+1 for some integer k). it's already possible to prove the theorem with all the facts we know.

replacing a and b with their respective even forms we get that a+b = 2k+2l (notice that we're using different variables for a and b, if it were the same variable it would only work if a=b or b=a), we can them factor out a 2 and get that a+b = 2(k+l) where k+l is an integer. and this is the exact definition of an even number (look at the last paragraph).

this is a very simple proof but it highlight very important concepts such as the structure of the proof. what we can assume looking at the theorem and how to approach it.

## proof by contrapositive

we can prove a theorem with the form P&rarr;Q by assuming &not;Q and trying to prove prove &not;P.

this is only possible because P&rarr;Q is equivalent to &not;Q&rarr;&not;P. (if you're not convinced you could make a truth table for both statements, see [logic](/notes/logic)).

theorem. let x be an integer. if if 7x+9 is even, then x is odd.

proof. we will prove the contrapositive. suppose x is even. this means x = 2k for some integer k. then 7x+9 = 7(2k)+9 = 14k+9 = 2(7k+4)+1 where 7k+4 is an integer. this shows that 7x+9 is odd. therefore 7x+9 is even. QED

here we used a bunch of the same concepts used in the direct proof. stating are it means for an number to be even or odd, using that new fact to derive more facts, and so on.

the more proofs you do the easier it will be to know which kind of technique to use, we can always try out both and see which one works the best. math is about exploration.
