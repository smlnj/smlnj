(* skeleton.sml
 *
 * Generated from skeleton.asdl by asdlgen.
 *)

structure SkeletonRep = struct
    type symbol = Symbol.symbol
    type sympath = SymPath.path

    datatype namespace = datatype Symbol.namespace
    type symbol_pkl = namespace * string
    type symset_pkl = symbol list
    type sympath_pkl = symbol list
    datatype modExp
      = Var of sympath
      | Decl of decl list
      | Let of decl list * modExp
      | Ign1 of modExp * modExp
    and decl
      = Bind of symbol * modExp
      | Local of decl * decl
      | Par of decl list
      | Seq of decl list
      | Open of modExp
      | Ref of SymbolSet.set
    (* convert a pickled symbol to a `Symbol.symbol` *)
    fun symbolFromPkl ((ns, n) : symbol_pkl) = (case ns
           of VALspace => Symbol.varSymbol n
            | TYCspace => Symbol.tycSymbol n
            | SIGspace => Symbol.sigSymbol n
            | STRspace => Symbol.strSymbol n
            | FCTspace => Symbol.fctSymbol n
            | FIXspace => Symbol.fixSymbol n
            | LABspace => Symbol.labSymbol n
            | TYVspace => Symbol.tyvSymbol n
            | FSIGspace => Symbol.fsigSymbol n
          (* end case *))
    (* convert a `Symbol.symbol` to the pickled symbol *)
    fun symbolToPkl sym = (Symbol.nameSpace sym, Symbol.name sym)
    fun spathFromPkl syms = SymPath.SPATH syms
    fun spathToPkl (SymPath.SPATH syms) = syms
    fun symsetFromPkl syms = SymbolSet.fromList syms
    fun symsetToPkl symSet = SymbolSet.listItems symSet

  end

(* hide the pickling-related types and code *)
structure Skeleton : sig
    type symbol = Symbol.symbol
    type sympath = SymPath.path
    datatype decl
      = Bind of symbol * modExp
      | Local of decl * decl
      | Par of decl list
      | Seq of decl list
      | Open of modExp
      | Ref of SymbolSet.set
    and modExp
      = Var of sympath
      | Decl of decl list
      | Let of decl list * modExp
      | Ign1 of modExp * modExp
  end = SkeletonRep

