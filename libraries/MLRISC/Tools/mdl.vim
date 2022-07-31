" Vim syntax file
" Language:	SML/NJ Machine Description Tool
" Maintainer:	Allen Leung <leunga@cs.nyu.edu>
" Last change:	Nov 21, 2000
" For version 3.0 MDL

" Remove any old syntax stuff hanging around
syn clear

syn keyword mdStatement	structure struct signature sig functor 
syn keyword mdStatement architecture end opcode instruction
syn keyword mdStatement	register cell cells cellset little big endian bits
syn keyword mdStatement	field fields vliw superscalar name version
syn keyword mdStatement	fun fn let in val rec of and raise handle as
syn keyword mdStatement	type eqtype datatype exception sharing where 
syn keyword mdStatement	withtype with abstype open local ordering
syn keyword mdStatement	is are format formats to encode encoding
syn keyword mdStatement	signed unsigned assembly at
syn keyword mdStatement	uppercase lowercase section delayslot nodelayslot
syn keyword mdStatement span dependent always never candidate semantics
syn keyword mdStatement nullified aggregable aliasing
syn keyword mdStatement called locations storage internal latency pipeline
syn keyword mdStatement		mc asm rtl padding cpu resource
syn keyword mdConditional	if else then case include
syn keyword mdRepeat		while infix infixr nonfix
"syn match mdLabel		"\#[a-zA-Z'][a-zA-Z0-9_']*\>" 

syn keyword mdTodo contained	TODO

" String
syn match   mdSpecial	contained "\\x\x\+\|\\\o\{1,3\}\|\\.\|\\$"
syn region  mdString	start=+"+ skip=+\\\\\|\\"+ end=+"+ contains=mdSpecial
syn region  mdChar	start=+#"+ skip=+\\\\\|\\"+ end=+"+ contains=mdSpecial

syn match  mdIdentifier		"\<[a-zA-Z][a-zA-Z0-9_']*\>"
syn match  mdType			"'\<[a-zA-Z][a-zA-Z0-9_']*\>"

"syn match  mdDelimiter		"[(){}\[\]]"
syn match  mdNumber		"\<\d\+\>"
syn match  mdNumber		"\<\d\+\.\d\+[eE]\~?\d\+\>"
syn match  mdWord		"0xw[0-9a-fA-F]+"
syn match  mdWord		"0w\d+"

" If you don't like tabs
"syn match mdShowTab "\t"
"syn match mdShowTabc "\t"

syn region mdComment	start="(\*"  end="\*)" contains=mdTodo
"syn region mdComment	start="{"  end="}" contains=mdTodo

syn keyword mdOperator	andalso orelse not div mod
syn keyword mdOperator	false true 
syn keyword mdType	char string int real exn bool word list option 
syn keyword mdType	array vector unit ref
syn keyword mdType	"\->"
syn keyword mdType	"\*"

"syn keyword mdFunction	fun 

syn sync lines=250

if !exists("did_md_syntax_inits")
  let did_md_syntax_inits = 1
  " The default methods for highlighting.  Can be overridden later
  hi link mdStatement			Statement
  hi link mdLabel			Label
  hi link mdConditional			Conditional
  hi link mdRepeat			Repeat
  hi link mdTodo			Todo
  hi link mdString			String
  hi link mdChar			String
  hi link mdNumber			Number
  hi link mdWord			Number
  hi link mdOperator			Operator
  hi link mdFunction			Function
  hi link mdType			Type
  hi link mdComment			Comment
  hi link mdStatement			Statement

"optional highlighting
  hi link mdDelimiter			Identifier

  "hi link mdShowTab			Error
  "hi link mdShowTabc		Error

  hi link mdIdentifier		Identifier
endif

let b:current_syntax = "md"

" vim: ts=8
