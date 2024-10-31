+++
date = "2024-04-09T00:00:00-03:00"
title = "ASCII"
+++

The man page for ASCII has a pretty neat table showing all ASCII characters, with both decimal and hexidecimal representations, and it's great and all but there's one problem, it's a the bottom of the manual page, making it less than ideal to use it as a quick reference.

So I created a small command-line application that does just that, displays an ASCII table.

## ii

The command is called _ii_, as in asc*ii*.

By default, *ii* shows the ASCII table using hexadecimal values:

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

It's also possible to specify hexadecimal output with the `-x` or `--hex` flags.

Using the `-d` or `--dec` flags, it outputs the ASCII table with decimal values instead:

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

## Source Code

The implementation is just over a hundred lines of ANSI C code, to install it just simply run:

```
% make && make install
```

By default, it installs the binary to `/usr/local/bin/` but it can be easily overriden with the `PREFIX` environment variable.

You can find the full source code at [this repository](https://sr.ht/~fkinos/ascii-table).
