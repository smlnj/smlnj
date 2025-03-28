# Linenoise for Standard ML

This library is an implementation of a TextIO.instream with history editing.
The implementation is loosely based on ideas from the the [linenoise
library](https://github.com/antirez/linenoise), with some ideas from
the [Swift version of the library](https://github.com/andybest/linenoise-swift).

## ANSI Output Sequences

We move the cursor using the [ANSI Escape Codes](https://en.wikipedia.org/wiki/ANSI_escape_code).

## Terminal Input Sequences

We handle arrow keys, which are the same as the ANSI escape codes.  The escape
sequences for other special keys depend on the terminal type.

https://en.wikipedia.org/wiki/ANSI_escape_code#Terminal_input_sequences
https://invisible-island.net/xterm/ctlseqs/ctlseqs.html

## TODO

Think about how to integrate into the **SML** stream IO APIs.  We might
create `TextPrimIO.reader` and `TextPrimIO.writer` values from a terminal.
These can then be used to create streams (we probably want to support
those directly).

Look at https://github.com/haskell/haskeline for ideas about API design.

