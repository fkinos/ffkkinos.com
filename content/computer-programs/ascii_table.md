+++
date = "2024-04-09T00:00:00-03:00"
title = "ASCII Table"
+++

some time ago, when i was playing with binary formats,
i was struggling a lot with ASCII values,
specially when represented with hexadecimal numbers.

around the same time,
someone over at mastodon (I don't remember who it was, sorry)
shared that the man page for ASCII had a neat table, including both decimal and hexidecimal representions.
And it's great, the only problem is that it's at the bottom of the manual, making it less than ideal to use it as a quick reference.

that's when I decided to build a small commandline application to make the same!

## asc*ii*-table
the binary for the application is called _ii_,
originally I wanted to call it _ascii_ but that name was already taken.

By default, _ii_ shows the ASCII table using hexadecimal values,
i made it that way because i wanted to practice reading hexadecimal,
and also the table itself is shorter.

```
% ii
   2 3 4 5 6 7
 -------------
0:   0 @ P ` p
1: ! 1 A Q a q
2: " 2 B R b r
3: # 3 C S c s
4: $ 4 D T d t
5: % 5 E U e u
6: & 6 F V f v
7: ' 7 G W g w
8: ( 8 H X h x
9: ) 9 I Y i y
A: * : J Z j z
B: + ; K [ k {
C: , < L \ l |
D: - = M ] m }
E: . > N ^ n ~
F: / ? O _ o DEL
```

using the `-d` or `--dec` flag, it displays the ASCII table with decimal values.
```
% ii --dec
  30 40 50 60 70 80 90 100 110 120
 ---------------------------------
0:    (  2  <  F  P  Z  d   n   x
1:    )  3  =  G  Q  [  e   o   y
2:    *  4  >  H  R  \  f   p   z
3: !  +  5  ?  I  S  ]  g   q   {
4: "  ,  6  @  J  T  ^  h   r   |
5: #  -  7  A  K  U  _  i   s   }
6: $  .  8  B  L  V  `  j   t   ~
7: %  /  9  C  M  W  a  k   u  DEL
8: &  0  :  D  N  X  b  l   v
9: '  1  ;  E  O  Y  c  m   w
```

## source Code
the implementation is just over a hundred lines of ANSI C code.
it includes a Makefile, all you need to do to install it is:

```
% make && make install
```

by default, it installs the binary to `/usr/local/bin/` but it can be easily overriden.

you can find the full source code on
[this repository](https://sr.ht/~fkinos/ascii-table) at [sr.ht](https://sr.ht).
