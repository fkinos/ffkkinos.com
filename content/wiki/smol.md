+++
date = "2024-10-31T00:00:00-03:00"
title = "Smol"
image = true
+++

![Screenshot of Smol applications running](/smol.png)

# Smol

Smol is a small collections of small applications for your desktop.

Currently Smol provides the following applications:

- a calendar
- a clock
- a rotating cube (because it's fun)
- a [PETSCII](https://en.wikipedia.org/wiki/PETSCII) painting program
- [a file explorer](/wiki/files)

## Source Code

All applications are written in ANSI C code using the SDL2 library, to install it just simply run:

```
% make && make install
```

By default, it installs the binary to `/usr/local/bin/` but it can be easily overriden with the `PREFIX` environment variable.

You can find the full source code at [this repository](https://sr.ht/~fkinos/smol/sources).
