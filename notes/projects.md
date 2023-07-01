SML/NJ Related Projects

----------------------

Development/maintenance projects for the SML/NJ compiler, tools, and libraries

The following might be potential tasks for new volunteers joining the SML/NJ Fellowship.

1. Reimplementation of the (classic) SML/NJ match compiler

	- presentation at ML Family Workshop, 2022

	- need to document in a tech report

	- [DBM: Done, 2021]


2. Design,implementation, and documentation of a new, improved prettyprinting library

	- synthesis of ideas from Oppen, PPML, Hughes-Wadler-Leijen

	- [DBM: (Done), 2022-3]


3. Revamping and modernizing the SML/NJ type checker

	- improved type error messages

	- new treatment of type variables and type constructor variables

	- [[ equality polymorphism elimination? (while retaining "equality types")]]

	- [DBM: 2023- ...]


4. Redesigning and reimplementing the SML/NJ module system

	- in conjunction with the reworking of the type system

	- Successor ML redesign

	- new formal semantics and its documentation

	- functor application "inlining" for optimization

	- [DBM: 2023- ...]


5. Reimplementation and simplification of the FLINT type system

	- revision of hash-consing scheme

	- simplified approach to type closures (replacing semi-Nadathur closures)

	- elimination/replacement of deBruijn type variables (using only conventional "named" type variables)


6. Revision or replacement of the FLINT middle end

	- Reppy-Shivers plans?

	- [JHR: ...]


7. Support for the Windows port of SML/NJ

	- platform-specific libraries and OS interfaces

	- maintain Windows installer

	- investigate building on Linux for Windows 

	- updating, documentation, etc.


8. Simplification and documentation of the SML/NJ build process


9. Revised compiler architecture

	- continuing evolution toward fully library-based compiler

	- more balanced support for interactive system and batch compilation

	- better support for stand-alone SML applications


10. New memory management in runtime system

	 - [JRH: ongoing]


11. Preservation, maintenance, improvement of MLRISC code generation library

	 - hedging the bet on llvm code generation


12. RISCV code generator

	 - based on llvm or MLRISC?


13. New or improved tools and libraries

	 - HTML5 support

	 - porting selected Python libraries?


14. Adding support for Unicode characters and strings


15. Package manager and distribution system for libraries

	 - Examples: Hackage for Haskell, OCaml package management?


16. Compiler distribution and installation

	 - maintain OS-specific compiler installers (Mac, Linux, Windows)

	 - building packages for Linux package managers

	 - Macports and Homebrew on macOS

	 - improved support and documenation for building from source
	 

17. Microsoft LSP (Language Server Protocol) support for SML/NJ

	 - interacts with front-end representations (types, absyn)
	   and therefore is related to projects 3 and 4.


18. Hand-made recursive descent parser with good syntax error messages

    - Does such a thing already exist? (e.g. in MLton or in Shwestrick's
      [smlfmt] (https://github.com/shwestrick/smlfmt])?
    

DBM, 2023.1.14