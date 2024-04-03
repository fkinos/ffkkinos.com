+++
date = "2024-03-28T00:00:00-03:00"
title = "Lisp and Linear Algebra"
toc = true
+++

While learning linear algebra I was calculating a bunch of vector operations by hand and I was getting a bit tired of that, that was when I tried to pratice Lisp a bit to see what I could do to make my life easier.

## Vectors

Lisp already has a data structure called vector, one-demensional arrays which can be accessed in constant time. We are not talking about this vector! This is linear algebra!!!

For this little experiment I'm going to represent vectors using regular lists.

## Operations

### Scalar Multiplication
The simplest operation I can think of is scalar multiplication, where you multiply all components in a vector by a scalar value, that is, just a real number.

```
(defun vec-scalar (vec n)
  (map 'list #'(lambda (x)
                 (* x n))
        vec))
```

The _map_ function expects the return type, here we're returning a list, a function that will run on each element of the list, a simple lambda that multiplies _x_ by _n_, and the sequence we want to apply our function to.

Calling our _vec-scalar_ function gives us the expected answer!

```
* (vec-scalar '(1 2 3) 5)
(5 10 15)
```

### Adding, Subtracting, and Multiplying Vectors
This is pretty similar to the _vec-scalar_ function we defined earlier, but this time we're using _mapcar_ instead of _map_.

```
(defun vec-add (a b)
  (mapcar #'(lambda (x y)
              (+ x y))
          a
          b))
```

_mapcar_ expects a function that will run on each pair, with the first component being the nth item in the list a and second component being the nth item in the list b.

Calling _vec-add_ should give you the following output:

```
* (vec-add '(1 2 3) '(4 5 6))
(5 7 9)
```

#### Vector Substraction
```
(defun vec-sub (a b)
  (mapcar #'(lambda (x y)
              (- x y))
          a
          b))
```

#### Vector Multiplication
```
(defun vec-mult (a b)
  (mapcar #'(lambda (x y)
              (* x y))
          a
          b))
```

### Dot Product

The dot product of two vectors is calculated by multiplying the nth item in list a with the nth item in list b and returning the sum of those products.

We just did something very similar to this on the last function using _mapcar_, we just need a way to sum all items in the resulting list, and there's exactly what _apply_ would give us.

```
(defun vec-dot (a b)
  (apply #'+
         (mapcar #'(lambda (x y)
                     (* x y))
                 a
                 b)))
```

The _apply_ function _applies_ some function, in this case _+_, to all items in the list. It's like running:

```
(+ 1 2 3)
```

But using the elements from the list _mapcar_ returns.

Calling _vec-dot_ should give you the following output:

```
* (vec-dot '(1 2 3) '(4 5 6))
32
```

## Macro Time!

The functions for addition, subtraction, multiplication are all very similar, with the only difference being the operation itself applied in the internal _lambda_ on the _map_ function. Let's create a macro to deal with that!

```
(defmacro vec-op (op a b)
  `(map 'list #'(lambda (x y)
                  (,op x y))
      ,a
      ,b))
```

Here we're getting the operation as a parameter, and we _unquote_ it inside the lambda passed to _map_, this is telling the macro to treat it as a function and not just as a symbol, we do the same for _a_ and _b_.

We can call this macro passing different functions to it, like the built-in _+_, _-_, and _*_ functions.

```
* (vec-op + '(1 2 3) '(4 5 6))
(5 7 9)
* (vec-op - '(1 2 3) '(4 5 6))
(-3 -3 -3)
* (vec-op * '(1 2 3) '(4 5 6))
(4 10 18)
```

Or even with a lambda created at call site.

```
* (vec-op (lambda (x y)
            (+ (* x x) (* y y)))
          '(1 2 3)
          '(4 5 6))
(17 29 45)
```

With this we've turned three functions that we doing basically the same thing into a more generic function that can do even more!

## Exercise for the reader

If you try to pass in anything other than a list to any of the functions we defined earlier we get an error saying it expected a list. What could we do to prevent this from happening?
