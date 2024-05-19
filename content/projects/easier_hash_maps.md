+++
date = "2024-05-09T00:00:00-03:00"
title = "Easier Hash Maps in Common Lisp"
toc = true
+++

After some time working with Common Lisp hash maps I created a small library to make working with them more straight-forward.

## Motivation

In languages like Python we can use this syntax to declare a new dictionary:
```
my_dict = {
  'name': 'hashmap',
  'done': False,
}
```

There's also a bunch of functions to something out of the hash maps,
like getting the value from a key,
inserting a new key with a value,
getting pairs of keys and values, etc.

I wanted something similar for Common Lisp without having to think too much about it.

## How To Use It

### Hash Map Declaration

We can declare a new hash map with this simpler syntax:
```
(defparameter *dict*
              (dict:new
                :name "felipe"
                :website "ffkkinos.com"
                :foo 1))
```

_dict_ expects key-value pairs like found in property lists.

### Inserting, Retrieving, and Removing

We can use the _dict-insert_ function to insert a new value,
or update an existing one:
```
* (dict:insert *dict* :email "example@mail.com")
"example@mail.com"
```

Similarly, to get the value of a key we can use the _dict-get_ function:
```
* (dict:value *dict* :name)
"felipe"
T
```

And to remove a key we can use the _dict-pop_ function:
```
* (dict:pop *dict* :name)
T
```

### Getting All Keys or Values

To get all keys from a hash map we can use the _dict-keys_ function:
```
* (dict:keys *dict*)
(:name :website :email :foo)
```

Similarly, we can use the _dict-values_ function to get all values:
```
* (dict:values *dict*)
("example@mail.com" 1 "ffkkinos.com" "felipe")
```

### Getting Key-Value Pairs

We can also get both the key and the value by using the _dict-items_ function:
```
* (dict:items *dict*)
((:email "example@mail.com") (:foo 1) (:website "ffkkinos.com") (:name "felipe"))
```

## Source Code

The source-code is available [at this repository](https://sr.ht/~fkinos/dict).

All functions include documentation strings,
so if in doubt you can always use the _documentation_ function to get more information and examples on how to use said function:
```
* (documentation 'dict:new 'function)
"dict provides a nicer syntax for declaring hash tables.

Example:

  (dict:new
    :a 1
    :b 2)
  ; => Creates a hash table with two keys (:a and :b) and values (1 and 2) respectively.

Equivalent to the following form:

  (let ((hashtable (make-hash-table :test 'equal)))
    (setf (gethash :a hashtable) 1)
    (setf (gethash :b hashtable) 2)
    hashtable)

"
```
