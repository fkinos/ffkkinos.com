+++
date = "2024-07-20T00:00:00-03:00"
title = "logic"
+++

![drawing showing various mathematical logic symbols](/logic.png)

mathematical logic is a way to precisely describe mathematical statements.

## notation

let's assign some symbols to make working with statements and the relation between statements easier.

it's pretty common to use letters starting from P to represents statements, so we could have a statement P standing for "it's raining" and a statement Q standing for "it's sunny".

when by themselves statements can be either true or false. it's either raining or not raining. it's either sunny or not sunny.

we can related different statements using logic connectives.

CONJUNCTION. P&and;Q (P and Q) is true when both P and Q are true. if it's sunny and raining then P&and;Q is true.

DISJUNCTION. P&or;Q (P or Q) is true when either P or Q are true. if it's sunny but not raining or raining but not sunny, P&or;Q is true.

NEGATION. &not;P (not P) is true exactly when P is false and vice versa. if it's sunny, &not;P means that it's not sunny.

## truth table

it's often handy to create truth tables to analyze when our statements are true or false.

we start by listing out all possible values of a statement. let's create a truth table for P.

P only has two possible values, true or false.

|*P*|
|---|
| 0 |
| 1 |

when analyzing more statements we need to add more lines to our table.

P and Q together have four possible combinations.

|*P*|*Q*|
|---|---|
| 0 | 0 |
| 0 | 1 |
| 1 | 0 |
| 1 | 1 |

as a general rule, n statements will produce a truth table with 2^n lines.

a cool tricky to fill up your truth table is to count from 0 to 2^n in binary.

we can also create truth tables for our logical connectives like the not, and, and or operators.

|*P*|*Q*|*&not;P*|*P&and;Q*|*P&or;Q*|
|---|---|--------|---------|--------|
| 0 | 0 | 1      | 0       | 0      |
| 0 | 1 | 1      | 0       | 1      |
| 1 | 0 | 0      | 1       | 1      |
| 1 | 1 | 0      | 1       | 1      |

if we have some logical reasoning like this:



{{< rawhtml >}}
<p>
  P&or;(&not;Q&and;Q)<br />P<br />&therefore; P
</p>
{{< /rawhtml >}}

meaning, P&or;(&not;Q&and;Q) is the statement we are trying to analyze, we later gather that P is true, therefore we conclude P.

we can use truth tables to make sure this is correct.

this truth table is organized in a few section, the first two columns are for our statements P and Q, the next one is for our premises, and the last one for our conclusion.

|*P*|*Q*|*P&or;(&not;Q&and;Q)*|*P*|*P*|
|---|---|---------------------|---|---|
| 0 | 0 | 0                   | 0 | 0 |
| 0 | 1 | 0                   | 0 | 0 |
| 1 | 0 | 1                   | 1 | 1 |
| 1 | 1 | 1                   | 1 | 1 |

if we spot a row where our premises are true but the conclusion is false that means that the argument is invalid, in our case we can see that when our premises are true our conclusion is also true, that means that our argument is valid.

## quantifiers

analyzing some statements only using truth tables seems pretty difficult, how could we tell if the statement x=5 is true or false.

this is where we use quantifiers, there's the universal quantifier and the existential quantifier.

UNIVERSAL. &forall;xP(x) (for all x's, P of x) is true when the statement P is true for _all_ values of x.

if we let P(x) stand for "x is a prime number", then &forall;xP(x) is false. all numbers are not prime.

EXISTENTIAL. &exist;xP(x) (there's at least one x such that P of x) is true when the statement P is true for at least one value of x.

if we let P(x) stand for the same statement as in the last example, &exist;xP(x) is true. there's at least one value of x that is prime.

## bounds

some statements might be true in some [sets](/notes/sets) but false in others, bounded quantifiers let's know what to focus on.

if we let P(x) stand for "x is bigger than or equal to zero".

the statement &forall;x&isin;&Nopf;P(x) (for all x in the natural numbers, P(x)) is true. all values of x in the natural numbers are bigger than or equal to zero.

the statement &forall;x&isin;&Zopf;P(x) (for all x in the integers, P(x)) is false. the integers include negative numbers which makes P(x) false.
