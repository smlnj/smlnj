# ml-ulex CM plugins

There are four ml-ulex related plugins for CM.  Each tool
has a SML source file that defines the tool and a CM file
for building the tool.

* `ml-ulex-tool` -- this is the tool for running the ml-ulex
  lexer generator on lexer specifications.

* `mllex-tool` -- this is the tool for running the ml-ulex
  lexer generator in ml-lex compatibility mode.

* `lex-ext` -- this tool maps "`.lex`" file extensions to ml-ulex
  files.  Note that it is incompatible with the `mllex-ext` tool.

* `mllex-ext` -- this tool maps "`.lex`" file extensions to ml-ulex
  in compatibility mode.  Note that it is incompatible with the `lex-ext`
  tool.
