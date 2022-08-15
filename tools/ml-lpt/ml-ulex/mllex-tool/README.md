# ml-ulex Compatibility-Mode CM Plugins

These are the `ml-ulex` plugins for using ml-ulex in
*compatibility* mode to process legacy `ml-lex`
specifications.  The directory `../tool` has the
plugins for processing `ml-ulex` specifications.

Each tool has a SML source file that defines the tool and a
CM file for building the tool.

* `mllex-tool` -- this is the tool for running the ml-ulex
  lexer generator in ml-lex compatibility mode.

* `lex-ext` -- this tool maps "`.lex`" file extensions to
  `ml-ulex`  in compatibility mode.  Note that it is incompatible
   with the `../tool/lex-ext` tool.
