# ml-ulex CM Plugins

These are the `ml-ulex` plugins for running ml-ulex from CM.
Note that the directory `../mllex-tool` has plugins
for using ml-ulex in *compatibility* mode to process
legacy `ml-lex` specifications.

Each tool has a SML source file that defines the tool and a
CM file for building the tool.

* `ml-ulex-tool` -- this is the tool for running the ml-ulex
  lexer generator on lexer specifications.

* `lex-ext` -- this tool maps "`.lex`" file extensions
  to `ml-ulex` files.  Note that it is incompatible with the
  `../mllex-tool/lex-ext` tool.
