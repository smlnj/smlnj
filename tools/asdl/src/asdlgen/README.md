## Implementation Notes

This file gives an overview of the implementation architecture of **asdlgen**.

### Roadmap

#### `driver/`
* `main.sml`

#### `common/`
* `sources.cm`
* `options.sml`
* `config.sml`

#### `front-end/`

* `sources.cm`

* `front-end.sml`

* `ast/`
     - `ast.sml`
     - `ident-fn.sml`
     - `prim-types.sml`

* `parser`
     - `asdl.grm` -- ML-Antlr grammar for ASDL
     - `asdl.lex` -- ML-ULex specification for ASDL
     - `parse-tree.sml` -- Parse-tree representation of ASDL
     - `parser.sml` -- Parser glue code

* `common/`
     - `error.sml` -- implements error reporting for the front-end

* `typechecker/` <br/>
  This directory contains code to typecheck an **ASDL** specification
  while converting the parse tree into an abstract syntax tree.
     - `env.sml`
     - `typecheck.sml`

* `views/` <br/>
  This directory contains the  definitions of the views that control
  how the back-ends access process the specification.
     - `common-view.sml`
     - `cxx-view.sml` -- implementes the C** view
     - `prop-names.sml`
     - `sml-view.sml` -- implementes the SML view
     - `view-base-fn.sml`
     - `view-base-sig.sml`
     - `view.sml`

#### `back-end` <br/>
The back-end of **asdlgen** is organized into target-language-specific
subdirectories.

* `util/`
    - `encoding.sml` -- represents a target-language independent encoding of
       **ASDL** types
    - `sort-decls.sml` -- sorts **ASDL** type declarations by dependency
       order
    - `string-subst.sml` -- substitution expansion for code fragments

* `cxx/` <br/>
  The back-end for **C++** code generation.  The **C++** backend generates code
  that uses the ``std::instream` and `std::outstream` types, which means that
  memory and file pickling are supported by the same set of functions.
    - `cxx.sml` -- a syntax-tree representation of a subset of the **C++** language
    - `gen-cxx.sml` -- the main entrypoint for the **C++** backend
    - `gen-pickle.sml` -- code generation for pickling
    - `gen-types.sml` -- code generation for the **C++** types that represent
      the **ASDL** types in a module.
    - `print-cxx.sml` -- printer for the representation defined in `cxx.sml`
    - `util.sml` -- various utility functions

* `sml/` <br/>
  The back-end for **SML** code generation.
    - `fragments/` <br/>
       This directory contains code fragments that get included in the generated
       code.  These are converted to string literals and get packaged up in the
       `fragments.sml` file.
       + `CATALOG` -- catalog of fragments
       + `io-util.in` -- file pickling operations for options and sequences
       + `pickle-util.in` -- memory pickling operations for options and sequences
       + `sexp-util.in`
    - `fragments.sml` -- this file is generated from the `fragments` directory
    - `gen-pickle-sig.sml` -- code generation for the common pickler signature
    - `gen-pickle-fn.sml` -- common code generation for memory and file picklers
    - `gen-sexp-pickle.sml` -- code generation for S-Expression pickler
    - `gen-sml.sml` -- the main entrypoint for the **SML** backend
    - `gen-types.sml` -- code generation for the **SML** types that represent
      the **ASDL** types in a module.
    - `print-sml.sml` -- printer for the representation defined in `sml.sml`
    - `sml.sml` -- a syntax-tree representation of a subset of the **SML** language
    - `util.sml`

### TODO

  * move SML name mangling to the SMLView module

