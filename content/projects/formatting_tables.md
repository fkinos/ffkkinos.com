+++
date = "2024-05-18T00:00:00-03:00"
title = "formatting tables in common lisp"
+++

## motivation

i've created a small library to format data in a tabular form,
i wanted something like this for a quite some time while working on a data-heavy project.

## how to use it

seisei (the library name) is super small,
it only export a single function called `format-table`.

it expects the output stream, and the data.

```
* (seisei:format-table t
    '(("X" income 50.0 "2024-05-18T00:00:00")
      ("Y" income 25.0 "2024-04-18T01:00:00"))
COL1 | COL2   | COL3 | COL4
X    | INCOME | 50.0 | 2024-05-18T00:00:00
Y    | INCOME | 25.0 | 2024-04-18T00:00:00
```

the data is expected to be a list of lists, each element in the lists will become a column.

## optional parameters

there are a few optional parameters to change the behavior of the `format-table` function.

### column labels

the `:column-label` parameter allows us to specify a label for each column in the table.

it expects a list of strings, the first string will label the first column, and so on.

```
* (defparameter +data+
    '(("X" income 50.0 "2024-05-18T00:00:00"))
      ("Y" income 25.0 "2024-04-18T00:00:00"))
+DATA+
* (seisei:format-table t +data+
    :column-label '("Description" "Type" "Amount" "Date"))
Description | Type   | Amount | Date
X           | INCOME | 50.0   | 2024-05-18T00:00:00
Y           | INCOME | 25.0   | 2024-04-18T00:00:00
```

### column alignment

the `:column-align` parameter allows us to specify how to align each column in the table.

there are three possible alignment options, left-aligned, right-aligned, or center-aligned, these are defined in a package constant which we can use when calling `format-table`.

```
* (defparameter +data+
    '(("X" income 50.0 "2024-05-18T00:00:00"))
      ("Y" income 25.0 "2024-04-18T00:00:00"))
+DATA+
* (seisei:format-table t +data+
    :column-label '("Description" "Type" "Amount" "Date")
    :column-align '(:left :left :right :left))
Description | Type   | Amount | Date
X           | INCOME |   50.0 | 2024-05-18T00:00:00
Y           | INCOME |   25.0 | 2024-04-18T00:00:00
```

now we see that the third column is aligned to the right while the other ones are aligned to the left.

### separator

by default, `format-table` uses the string `"|"` as the column separator.

we can override that by providing our own string to be used instead, using the `:separator` parameter.

```
* (defparameter +data+
    '(("X" income 50.0 "2024-05-18T00:00:00"))
      ("Y" income 25.0 "2024-04-18T00:00:00"))
+DATA+
* (seisei:format-table t +data+
    :column-label '("Description" "Type" "Amount" "Date")
    :column-align '(:left :left :right :left)
    :separator "@")
Description @ Type   @ Amount @ Date
X           @ INCOME @   50.0 @ 2024-05-18T00:00:00
Y           @ INCOME @   25.0 @ 2024-04-18T00:00:00
```

now we see that each column is separated by the "@" string instead of the "|" default one.

## source code

the source-code is available [at this repository](https://git.sr.ht/~fkinos/seisei).

`format-table` function provides a compreehenvisve documentation string which can be accessed
so if in doubt you can always use the documentation function to get more information and examples on how to use it:
```
Outputs DATA as a table to STREAM.

DATA must be a list of lists.

COLUMN-LABEL and COLUMN-ALIGN must have the same length as the number of columns in the table.
If not provided, COLUMN-LABEL and COLUMN-ALIGN default to COL1, COL2, ..., COLN and :left, respectively.

Example:

  (format-table t '(("X" 10 5)
                    ("Y" 5 3))
    :column-label '("VECTOR" "X" "Y")
    :column-align '(:left :right :right))
  =>
  VECTOR |  X | Y
  X      | 10 | 5
  Y      |  5 | 3

```
