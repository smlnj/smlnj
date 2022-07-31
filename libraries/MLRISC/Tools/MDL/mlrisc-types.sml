(*
 * MLRISC specific things are abstracted out here in this module.
 *)

functor MLRiscTypes 
  (structure Comp : MDL_COMPILE
   structure RTL  : MLTREE_RTL
  ) : MLRISC_TYPES =
struct

   structure Ast    = Comp.Ast
   structure AstPP  = Comp.AstPP
   structure TU     = Comp.TypeUtils
   structure RTL    = RTL
   structure T      = RTL.T
   structure RW     = Comp.Rewriter
   structure C      = CellsBasis

   open Ast Comp.Util Comp.Error

   val t2s  = PP.text o AstPP.ty
   val e2s  = PP.text o AstPP.exp
   val SKIP = RW.noRewrite

   (* does this type has special meaning in an instruction representation?  *)
   fun isSpecialRepType t =
   let fun isSpecial t =
       case TU.deref t of 
         CELLty _                  => true  (* cell types are special *)
       | IDty(IDENT(_,"int"))      => true
       | IDty(IDENT([],"operand")) => true
       | IDty(IDENT(_,"cellset"))  => true
       | _ => false

       val found = ref false
 
       fun ty _ t = (if isSpecial t then found := true else (); t)  
       val _ = #ty(RW.rewrite{ty=ty,exp=SKIP,decl=SKIP,sexp=SKIP,pat=SKIP}) t
   in  !found
   end

   (* return the real representation type of an rtl argument *)
   fun representationOf(rtlName, arg, loc, ty) =
   let fun err() =
          (errorPos(loc,
               "'"^arg^"' in rtl "^rtlName^" has an illegal type "^t2s ty);
           (0, "bits")
          )
   in  case TU.deref ty of
         IDty(IDENT([],"operand"))             => (0, "operand")
       | IDty(IDENT([],"label"))               => (0, "label")
       | IDty(IDENT([],"region"))              => (0, "region")
       | APPty(IDENT([],"operand"), [INTVARty n]) => (n, "operand")
       | APPty(IDENT([],"bits"), [INTVARty n]) => (n, "bits")
       | ty => err()
   end
 
   fun representationOf(rtlName, arg, loc, ty) =
   let fun err() =
          (errorPos(loc,
               "'"^arg^"' in rtl "^rtlName^" has an illegal type "^t2s ty);
           (0, "bits")
          )
   in  case TU.deref ty of
         IDty(IDENT([],"operand"))                => (0, "operand")
       | IDty(IDENT(_,"label"))                   => (0, "label")
       | IDty(IDENT([],"region"))                 => (0, "region")
       | APPty(IDENT([],"operand"), [INTVARty n]) => (n, "operand")
       | APPty(IDENT([],"bits"), [INTVARty n])    => (n, "cell")
       | ty => err()
   end

   (* Given the actual represention of an rtl argument, 
    * insert coercion if possible.
    *)
   fun insertRepCoercion(exp, ty) = 
       (case (exp, TU.deref ty) of
         (T.ARG(_,k,_), IDty(IDENT([],"int")))     => k := T.REP "int"
       | (T.ARG(_,k,_), IDty(IDENT(_,"label")))    => k := T.REP "label"
       | (T.ARG(_,k,_), IDty(IDENT([],"operand"))) => k := T.REP "operand"
       | (T.$(_,_,T.ARG(_,k,_)), CELLty _) => k := T.REP "cell"
       | _ => ()
       )

   fun ofCellKind(T.$(_,k,_),CELLdecl{id, ...}) = 
          (case C.cellkindToString k of
             "CELLSET" => true
           | k => k = id 
          )
     | ofCellKind(T.ARG _,CELLdecl{id, ...}) = false
     | ofCellKind(_, _) = false


    (*
     * A database of all special types
     *)
    datatype howto = 
       HOWTO of 
        { rep           : string, (* name of representation *)
          isSSAValue    : bool,   (* is it a value in SSA form? *)
          mlType        : Ast.ty, (* type in ML *)
          isConst       : bool,   (* if so, is it always a constant? *) 
          isMultiValued :         (* if a value can it take more than one *)
             Comp.md -> bool
        } 
 
     val howtos = ref [] : howto list ref
 
     fun findRep r =
         case List.find (fn HOWTO{rep, ...} => rep = r) (!howtos) of 
           SOME(HOWTO howto) => howto
         | NONE => fail("bug: representation "^r^" not known")
 
    (*---------------------------------------------------------------------
     * 
     * Code generation magic
     *
     *---------------------------------------------------------------------*)
     fun isConst(T.REP rep) = #isConst(findRep rep)

    (*---------------------------------------------------------------------
     * 
     * Okay, now specify all the types that we have to handle.
     *
     *---------------------------------------------------------------------*)
     fun no _ = false
     fun yes _ = true
     fun bug _ = fail("unimplemented")
 
     val _ = howtos :=
       [HOWTO{rep           = "label",
              isSSAValue    = false,
              mlType        = IDty(IDENT(["Label"],"label")),
              isConst       = true,
              isMultiValued = no
             },
 
        HOWTO{rep           = "int",
              isSSAValue    = true,
              mlType        = IDty(IDENT([],"int")),
              isConst       = true,
              isMultiValued = no
             },
 
        HOWTO{rep           = "operand",
              isSSAValue    = true,
              mlType        = IDty(IDENT(["I"],"operand")),
              isConst       = false,
              isMultiValued = yes
             },
 
        HOWTO{rep           = "cellset",
              isSSAValue    = true,
              mlType        = IDty(IDENT(["C"],"cellset")),
              isConst       = false,
              isMultiValued = yes
             }
       ]

   (*---------------------------------------------------------------------
    * 
    * Generate an expression for performing the appropriate conversion
    *
    *---------------------------------------------------------------------*)
   datatype conv = IGNORE | CONV of string | MULTI of string

   structure DescMap = RedBlackMapFn(type ord_key = string
                                     val compare = String.compare)
   fun getOpnd desc = 
   let val tbl =
           foldr (fn ((rep,conv), tbl) =>
                    DescMap.insert(tbl, rep, conv)) DescMap.empty desc

       fun mkConvFun(rep,conv) = 
           "fun get_"^rep^"(x,L) = "^
             (case conv of 
               IGNORE => "L"
             | CONV f => f^"::L"
             | MULTI f => f^"@L"
             )
       fun mkConvFun0(rep,conv) = 
           "fun get_"^rep^"'(x) = "^
             (case conv of 
               IGNORE => "[]"
             | CONV f => "["^f^"]"
             | MULTI f => f
             )
       val decl = $(map mkConvFun desc @ map mkConvFun0 desc)

       fun apply(rep, this, rest) = APP("get_"^rep,TUPLEexp[this,rest])

       fun getIt(rep, this, rest) = 
           case (DescMap.find(tbl, rep), rest) of
             (NONE, _) => fail("getOpnd: "^rep^" is not defined")
           | (SOME IGNORE, _)   => rest
           | (SOME(CONV _), _)  => apply(rep, this, rest)
           | (SOME(MULTI conv), LISTexp([],NONE)) => APP("get_"^rep^"'", this)
           | (SOME(MULTI _), rest) => apply(rep, this, rest)

       fun get(this,T.$(_,k,_),rest) =  
             if C.cellkindToString k = "CELLSET" then
                 getIt("cellset",this,rest)
             else
                 getIt("cell",this,rest)
          | get(this,T.ARG(_,ref(T.REP rep),_),rest) = getIt(rep,this,rest)
          | get(_, e, _) = fail("MLRiscTypes.get: "^RTL.Util.rexpToString e)

   in  { decl= decl,
         get = get 
       }
   end

end
