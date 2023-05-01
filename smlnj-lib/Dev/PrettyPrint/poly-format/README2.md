# PrettyPrint Library

## Changes from Version 8.4

This version of the library is stripped down from the version developed
by Dave MacQueen for the SML/NJ compiler.  The original version has a
lot of extra functions that are primarily useful for pretty-printing
SML/NJ code, but are not generally useful --- these were removed.  Also
removed the rendering

## Devices, Styles, and Tokens

This version of the library borrows the concepts of device, style, and
token from the PP library.

### Styles

Styles are represented by an application-specific type.  The user of the
pretty printer specifies how these are interpreted when defining a device,
since the rendering of a style will depend on the kind of device.

