+++
date = "2024-11-12T00:00:00-03:00"
title = "Paint"
image = true
+++

![Screenshot of a PETSCII drawing program](/paint.png)

# Paint

Paint is a [PETSCII](https://en.wikipedia.org/wiki/PETSCII) drawing program,
each character is typable on a conventional keyboard.

## How To Use It

You're presented with a 40x25 canvas, a cursor, and a lot of characters, you can type any number, lowercase and uppercase letter to change your brush, the cursor is set to whatever character was just typed, pressing space paints the brush to the canvas.

Moving the cursor to the edges of the canvas loops it to the opposite edge.

Using `Alt+0-9` you can change the foreground color a character is painted on, using `Alt+Shift+0-9` you can change the background color a character is placed on.

### Shortcuts

|                                  |                                                                          |
|----------------------------------|--------------------------------------------------------------------------|
| `Arrow Keys` | Moves the cursor up, down, left, and right accordingly                   |
| `Space`                          | Paints selected character to canvas                                      |
| `Alt+0-9`                        | Changes character foreground color                                       |
| `Alt+Shift+0-9`                  | Changes character background color                                       |
| `Ctrl`                           | Alternates between the default character set and the extra character set |

## Source Code

Files is written in ANSI C code using the SDL2 library, to install it just simply run:

```
% make && make install
```

By default, it installs the binary to `/usr/local/bin/` but it can be easily overriden with the `PREFIX` environment variable.

You can find the full source code at [this repository](https://git.sr.ht/~fkinos/paint).
