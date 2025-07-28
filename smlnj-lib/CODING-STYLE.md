# SML/NJ Library Coding Style

The SML/NJ Library follows the naming conventions of the **Standard ML Basis
Library** (these are described in the version of the Basis Library specification
published by Cambridge University Press).  We also have conventions for naming
files.

## Files

We use all lower-case with dashes ("`-`") to separate words in a filename.  We
typically put one SML module/signature per file, but occasionally include
both a named signature and module in the same file.  We use a "`-fn`" suffix
for files containing functors and a "`-sig`" siffix for files containing
signatures.  In some libraries, the convention is to use "`.sig`" as the extension
for files that contain only a signature and "`.fun`" for files that contain
functor definitions.

## Orthographic conventions

We largely follow the orthographic conventions specified for SML Basis (and used
by the SML/NJ Library).

1. Alphanumeric value identifiers are in mixed-case, with a leading lower-case
   letter.

2. Type identifiers are all lowercase, with words separated by underscores ("`_`").

3. Signature identifiers are all upper case, with words separated by underscores ("`_`").

4. Structure and functor identifiers are in mixed-case, with the initial letters
   of words capitalized.

5. Functor names have "`Fn`" as a suffix (*e.g.*, `HashTableFn`)

6. Alphanumeric datatype constructors follow either the signature convention (3)
   or the structure convention (4).

## Code Layout

The basic unit of indentation is two spaces.  We indent indent the `val` and
`fun` definitions of a `let` expression at the same level as the `in` and `end`.
For example,
``` sml
    let
    val x = ...
    in
      ...
    end
```
If there is room, we usually put the `let` on the same line as its parent.
``` sml
    fun f x = let
          val y = ...
          in
            ...
          end
```
or
``` sml
    if (x < 0)
      then let
        val y = ...
        in
          ...
        end
      else ...
```

## Comments

Each file should have a header comment that identifies the name of the file
and includes the copyright notice.  In an ideal world, the header comment will
also include information about what the code in the file does (but not how it does
it).

Types and functions in signatures should also be commented.

## Miscellany

Functions and data constructors that take multiple arguments of the same type (and
where the order is not obvious) should use a labeled record as their argument.

For case expressions, we "terminate" them with an "`(* end case *)`" comment.  E.g.,

``` sml
    case e
     of [] => ...
      | x::xs => ...
    (* end case *)
```