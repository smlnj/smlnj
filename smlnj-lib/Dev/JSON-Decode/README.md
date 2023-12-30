# JSON Decoding

This directory holds an implementation of JSON-value decoding combinators.
It is inspired by the Elm [JSON.Decode](https://package.elm-lang.org/packages/elm-lang/core/5.1.1/Json-Decode#data-structures)
and [Json.Decode.Pipeline](https://github.com/NoRedInk/elm-json-decode-pipeline/) modules.

It needs a few more combinators and documentation before being added to the
JSON library.

## Discussion

The `JSONUtil` module uses exceptions to signal errors, whereas this module
follows the Elm design and uses the `result` datatype to explicitly propagate
errors.  We should probably try to have consistency between the two. Some
choices:

* modify the `JSONUtil` structure to also provide versions of functions that
  return the `result` datatype; e.g.,
``` sml
val asInt : JSON.value -> int result
```

* modify the decode interface to just propagate exceptions (like `JSONUtil`).

