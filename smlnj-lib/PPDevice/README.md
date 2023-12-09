# Pretty-Printing Device Library

This library is an "internal" library that is meant to be shared by
the two pretty-printer libraries (pp-lib.cm and prettyprint.cm).

## The `PP_DEVICE` Signature

The `PP_DEVICE` signature is the base signature for modules that implement
pretty-printing devices.  Such modules will extend the `PP_DEVICE` signature
with additional operations for creating devices, etc.  The `PP_DEVICE`
signature defines three basic types: `device`, `style`, and `token` that
are described below.

### Devices

A "device" is an abstraction of a consumer of pretty-printing
commands.  It is a stateful object that maintains a stack of
styles, the line width that controls formatting of the output,
and other information.  It supports output operations for
indentation, whitespace, newlines, and both strings and tokens.

### Styles

Styles are a representation of output attributes (e.g., font, color,
underlining, etc.) that are supported by a given device.  The particular
set of attributes is device dependent.  For example, the ANSI Terminal
device (`ANSITermDev`) supports most of the standard
[ANSI SGR](https://en.wikipedia.org/wiki/ANSI_escape_code#SGR_(Select_Graphic_Rendition)_parameters),
while the basic text devices (`CharBufferDev` and `SimpleTextIODev`)
do not support any styling of the output.

### Tokens

Tokens are also a device-specific abstraction.  They provide support for
rendering glyphs, such as UTF-8 encoded characters) and small images.

## Application-Specific Pretty Printing

Using styles and tokens in a pretty-printer is application specific (e.g., one
might want to define a style for comments or use a token to represent the
gradient operation ∇).  The two pretty-printing libraries have different
approaches to how to use styles and tokens in a pretty printer.  See the
per-library documentation for more information.
