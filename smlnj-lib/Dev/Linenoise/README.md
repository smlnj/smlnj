# Linenoise for Standard ML

This library is an implementation of a TextIO.instream with history editing.
The implementation is loosely based on ideas from the the [linenoise
library](https://github.com/antirez/linenoise), with some ideas from
the [Swift version of the library](https://github.com/andybest/linenoise-swift).

## TODO

Think about how to integrate into the **SML** stream IO APIs.  We might
create `TextPrimIO.reader` and `TextPrimIO.writer` values from a terminal.
These can then be used to create streams (we probably want to support
those directly).
