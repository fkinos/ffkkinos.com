+++
date = "2024-11-12T00:00:00-03:00"
title = "Files"
image = true
+++

![Screenshot of Files running](/files.png)

# Files

Files is a file explorer program, like a file manager but without managing anything.

It's pretty handy for exploring your forgotten file structure as doing that with `cd` and `ls` can become very tiring.

## How It Works

It's a simple loop, it fetchs files from a directory, once a new directory is selected it fetchs its files, and so on.

## Source Code

Files is written in ANSI C code using the SDL2 library, to install it just simply run:

```
% make && make install
```

By default, it installs the binary to `/usr/local/bin/` but it can be easily overriden with the `PREFIX` environment variable.

You can find the full source code at [this repository](https://git.sr.ht/~fkinos/files).
