+++
date = "2024-07-20T00:00:00-03:00"
title = "Mathematical Logic and Logic 'Calculus'"
+++

Mathematical Logic helps us analyze the logical structure of mathematical statements, allowing us to ignore the specifics about statements and focus on their overall structure and relationship.

## Propositions

Logical statements or proposition are statements which can either be true or false, not both nor neither. It must be affirmative and declarative, not interrogative nor exclamatory.

The following are logical propositions:

- "It's raining."
- "It's snowing."
- "It's not sunny outside."

The following are not logical propositions:

- "Is it raining?"
- "1/9+9"

To make it easier to talk about different propositions without repeating them every time, we can assign each proposition to a letter, by convention we start with the letters P, Q, R, and so on.

So instead of saying the statement "It's raining." over and over, we could say that P stands for the statement "It's raining" and them we would just use P whenever we wanted to talk about this particular statement.

## Logical Connectives

Logical Connectives are what glues logical statements together, connectives can tell us how statements are linked together and how that affects their logical values.

### Negation

Perhaps the simplest connective is *not*, *not* negates whatever statement comes after it.

Consider the following propositions:

- "It's raining."
- "It's not sunny outside."

The negation of these propositions are:

- "It's not raining."
- "It's sunny outside."

Simbolically we could write that P stands for "It's raining" and the negation of P is denotated by *not* P, or ~P, or &not;P.

### Conjunction

The conjunction connective allows us to express the idea that two statement are both true.

Consider the following propositions:

- "It's raining."
- "It's snowing."

The conjunction of these propositions is:

- "It's raining and it's snowing."

Simbolically we could write that P stands for "It's raining." and that Q stands for "It's snowing." and the conjunction of P and Q is denotated by P *and* Q, or P&and;Q.

### Disjunction

The disjunction connective is pretty similar to conjunction but instead of requiring both statements to be true it's okay with only one of them being true.

Consider the following propositions:

- "It's raining."
- "It's snowing."

The disjunction of these propositions is:

- "It's raining or it's snowing."

Simbolically we could write that P stands for "it's raining." and that Q stands for "It's snowing." and the disjunction of P and Q is denotated by P *or* Q, or P&or;Q.

### Conditional

The conditional connective allows us to express the idea that if P is true, then Q is true. In other words P implies Q.

Consider the following propositions:

- "It's raining."
- "It's snowing."

The conditional of these propositions is:

- "If it's raining, then it's snowing."

Simbolically we could write that P stands for "it's raining." and that Q stands for "It's snowing." and the conditional of P and Q is denotated by P *imples* Q, or P&rarr;Q.

### Biconditional

The biconditional connective allows us to express the same idea the conditional connective expresses by going both ways, P implies Q and Q implies P.

Consider the following propositions:

- "It's raining."
- "It's snowing."

The biconditional of these propositions is:

- "It's raining if and only if it's snowing."

Simbolically we could write that P stands for "it's raining." and that Q stands for "It's snowing." and the biconditional of P and Q is denotated by P *if and only if* Q, or P&harr;Q.

## Truth Tables

While analyzing the validity of logical connectives it might be hard to consider all possible combinations of true and false values of each proposition involved. For making this easier we can make use of truth tables.

A truth table is a simple table with columns for all the propositions and rows for all possible values those propositions could take.

Let's construct our first truth table, let's consider the propositions P and &not;P.:

|**P**|**&not;P**|
|-----|----------|
|  F  | T        |
|  T  | F        |

We can see that whenever P is true &not;P is false and vice versa.

Let's take a look at a slightly more complex table:

|**P**|**Q**|**P&and;Q**|
|-----|-----|-----------|
|  F  |  F  |  F        |
|  F  |  T  |  F        |
|  T  |  F  |  F        |
|  T  |  T  |  T        |

It might take some getting used to to convince yourself that these are all the possible combinations of P and Q. As a general rule our truth table will have 2 to the *n* rows, where *n* is the number of propositions included.

In this truth table we can see that P&and;Q is only true when both P and Q are true.

Here are the truth tables for P&or;Q, and P&rarr;Q and Q&rarr;P:

|**P**|**Q**|**P&or;Q**|
|-----|-----|----------|
|  F  |  F  |  F       |
|  F  |  T  |  T       |
|  T  |  F  |  T       |
|  T  |  T  |  T       |

In this table we can see that P&or;Q is only false when both P and Q are false.

|**P**|**Q**|**P&rarr;Q**|**Q&rarr;P**|
|-----|-----|------------|------------|
|  F  |  F  |  T         | T          |
|  F  |  T  |  T         | F          |
|  T  |  F  |  F         | T          |
|  T  |  T  |  T         | T          |

Here we see that P implies Q or Q implies P is only false when the antecedent is true but the consequent is false.

We can also combine all connectives into more complex expressions like (P&or;Q)&or;P, and ((P&rarr;Q)&or;Q)&and;P.

As an exercise, give it a try at constructing truth tables for these last two more complex expresssions. To make it easier you could split the entire expression into simpler ones, each on its own column, with the entire expression in the last column.

### Analizyng Statements with Truth Tables

## Existencial and Universal Quantifiers

### There exists

### For all

## Bounds
