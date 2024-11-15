+++
date = "2024-11-12T00:00:00-03:00"
title = "paint"
image = true
+++

![Screenshot of a PETSCII drawing program](/paint.png)

# Paint is a drawing program.

Paint is a graphical [PETSCII](https://en.wikipedia.org/wiki/PETSCII) drawing program,
using the character set used on Commodore machines but each character is typable on a conventional keyboard.

You're presented with a 40x25 canvas and a cursor, typing any number, symbol, lowercase or uppercase letter changes your brush, pressing space paints the selected brush to the canvas.

Holding `Ctrl` changes the current brush to the corresponding character in the extra character set, which contains more drawing characters like boxes and lines.

Using `Alt+0-9` you can change the foreground color a character is painted on, using `Alt+Shift+0-9` you can change the background color a character is placed on.

## Source Code

Files is written in ANSI C code using the SDL2 library, to install it just simply run:

```
% make && make install
```

By default, it installs the binary to `/usr/local/bin/` but it can be easily overriden with the `PREFIX` environment variable.

You can find the full source code at [this repository](https://git.sr.ht/~fkinos/paint).
