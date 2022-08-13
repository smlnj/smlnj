This directory tree contains documentation for the
various libraries that comprise the **Standard ML of
Ney Jersey Library**.  The documentation is written
using [AscciDoctor](https://asciidoctor.org).

## Naming conventions

### File naming conventions

All source files have the `.adoc` file suffix.

Each library `Foo`, with CM file `foo-lib.cm`, has
its documentation in the `Foo` directory and has
a root documentation file named `Foo/foo-lib.adoc`.

Files that document modules (*i.e.*, signatures,
structures, or functors) begin with a prefix, which
is one of `sig-`, `str-`, or `fun-`, followed by the
module name and the `.adoc` file suffix.  For example,
consider the the `ORD_SET` signature.  It's documentation
will live in a file named `sig-ORD_SET.adoc`.  That file
will document the signature as well as its various instances
(*e.g.*, `AtomSet`, `RedBlackSetFn`, *etc*.).

### Specification naming conventions

The description of a specification has the basic form

    kind ':' [ owner '.' ] name

where `kind` is one of `str` (for substructures), `type`
for type and datatypes, `con` for data constructors,
`fld` for record fields, `exn` for exception constructors,
and `val` for value identifiers.  The optional `owner`
(followed by a period) is used for data constructors and
record fields, where the owner is the type that they
are part of.

Some examples:

	xref:#fld:point.x[x]
		-- local reference to `x` field of point type

	xref:str-Vec3D.adoc#val:cross[cross]
		-- cross-file reference to `cross` function in
		   the Vec3D structure.

Note that we use `xref:` instead of `link:`, because the latter
is specific to HTML generation, whereas the former applies to
PDF as well.

## External references

One can refer to entities in the SML Basis Library using the
syntax

    {sml-basis-url}/file.html#anchor[ ... ]

For example, we can cross reference the `Domain` exception in the
`General` structure as follows:

    {sml-basis-url}/general.html#SIG:GENERAL.Domain:EXN[`Domain`]

## Odds and ends

For inline code enclosed in back quotes, it is necessary to escape
the function type constructor (++\->++); otherwise, the entitie
`&#8594;` will be inserted in the HTML.

