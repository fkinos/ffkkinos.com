+++
date = "2024-03-28T00:00:00-03:00"
title = "Lisp and Linear Algebra"
toc = true
+++

While learning Linear Algebra and calculating a bunch of vector operations by hand I was getting a bit tired,
that's when I thought about making my life easier with a bit of Common Lisp code.

## Vectors

For this little experiment I'm going to represent vectors using regular lists.

## Operations

### Scalar Multiplication

The simplest operation I can think of is scalar multiplication,
where you multiply all components in a vector by a scalar value,
that is, just a real number.

```
(defun v@ (u alpha)
  "Returns scalar multiplication of U by ALPHA."
  (map 'list #'(lambda (x)
                 (* x alpha))
        u))
```

_map_ expects a return type (here we're returning a list),
a function that will run on each element of the list (here we're using simple lambda that multiplies _x_ by _alpha_),
and the sequence we want to apply our function to, which is _u_.

Calling _v@_ gives us the expected answer:
```
* (v@ '(1 2 3) 5)
(5 10 15)
```

### Operations on Two Vectors

Next we will implement adding, subtracting, and multiplying vectors.

The _v+_ function is pretty similar to the _v@_ function we defined earlier:
```
(defun v+ (u v)
  "Returns the addition of the vectors U and V."
  (mapcar #'(lambda (x y)
              (+ x y))
          u
          v))
```

_mapcar_ expects a function that will run on each pair,
with the first value being the nth component in a vector U,
and second value being the nth component in a vector V.

Calling _v+_ should give you the following output:
```
* (v+ '(1 2 3) '(4 5 6))
(5 7 9)
```

The functions for substraction and multiplication are pretty much the same.

#### Vector Substraction

```
(defun v- (u v)
  "Returns the subtraction of the vectors U and V."
  (mapcar #'(lambda (x y)
              (- x y))
          u
          v))
```

#### Vector Multiplication

```
(defun v* (u v)
  "Returns the multiplication of the vectors U and V."
  (mapcar #'(lambda (x y)
              (* x y))
          u
          v))
```

### Dot Product

The dot product of two vectors is calculated by multiplying the nth component in a vector U with the nth component in a vector V,
and returning the sum of those products.

```
(defun v. (u v)
  "Returns the dot product between the vectors U and V."
  (apply #'+
         (mapcar #'(lambda (x y)
                     (* x y))
                 u
                 v)))
```

The only difference between _v._ and the previous functions is the use of _apply_.
_apply_ _applies_ some function, in this case _+_, to all items in the list.

It's like running:
```
(+ 1 2 3)
```
But using the elements from the list _mapcar_ returns.

Calling _v._ should give you the following output:
```
* (v. '(1 2 3) '(4 5 6))
32
```

### Modulus, or Magnitude

To get the modulus, or magnitude of a vector we get the square root of the sum of all components squared.

```
(defun v-modulus (u)
  "Returns the modulus, or magnitude of the vector U."
  (sqrt (apply #'+
               (map 'list #'(lambda (n)
                              (expt n 2))
                    u))))
```

Here we see the use of the _expt_ function,
it expects a base and a exponent.

Calling _v-modulus_ should give you the following output:
```
* (v-modulus '(1 2))
2.236068
```

### Getting The Angle Between Two Vectors

Getting the angle between two vectors is a bit more complex.

We need to get the arccosine of the dot product of U and V over the product of the modulus of U and V. A bit long-winded isn't it, I find the code a bit easier to read.

```
(defun v0 (u v)
  "Returns the angle between the vectors U and V."
  (acos (/ (v. u v)
           (*
            (v-modulus u)
            (v-modulus v)))))
```

The code is almost a translation of the explanation above,
luckily for us,
Common Lisp provides an arccosine function called _acos_.

Calling _v0_ should give you the following output:
```
* (v0 '(1 0) '(0 3))
1.5707964
```

And that's probably not what you expected, right?
That's because it's returning the angle in radians,
so all that's left to do is create a function to convert radians to degrees.
```
(defun radians-to-degress (radians)
  "Converts RADIANS to degrees."
  (nth-value 0 (round (* radians (/ 180 PI)))))
```

We can do the conversion by multiplying the angle in radians by 180 over PI.

The _round_ function _rounds_ the resulting value,
_round_ returns multiple values,
the rounded number and the remainder.

Since we only care about the rounded number we can ignore the remainder,
the _nth-value_ function let's do exactly that!
Getting the zeroth return value from _round_.

Let's modify _v0_ to make use of the _radians-to-degrees_ function:
```
(defun v0 (u v)
  "Returns the angle between the vectors U and V."
  (radians-to-degress
   (acos (/ (v. u v)
            (*
             (v-modulus u)
             (v-modulus v))))))
```

Now _v0_ should give the answer in degrees, like we expected:
```
* (v0 '(1 0) '(0 3))
90
```

## Conclusion

To me that was a pretty good exercise,
making me understand both Linear Algebra and Common Lisp a bit better.
I hope it was useful for you too!
