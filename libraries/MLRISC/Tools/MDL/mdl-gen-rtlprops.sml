(*
 * Generate the <arch>RTLProps functor.
 * This structure extracts semantics and dependence 
 * information about the instruction set needed for SSA optimizations.
 *)

functor MDLGenRTLProps(RTLComp : MDL_RTL_COMP) : MDL_GEN_MODULE2 =
struct

   structure RTLComp = RTLComp
   structure Comp    = RTLComp.Comp
   structure M       = RTLComp.MLRiscTypes
   structure Consts  = Comp.Consts
   structure Ast     = Comp.Ast
   structure Env     = Comp.Env
   structure Tr      = Comp.Trans
   structure RTL     = RTLComp.RTL
   structure T       = RTL.T
   structure C       = CellsBasis

   open Ast Comp.Util Comp.Error

   exception Undefined
   exception NotFound

   (* Function to make a new RTL *)
   val makeNewRTL = IDexp(IDENT(["RTL"],"new"))

   (*------------------------------------------------------------------------
    *
    * Generate a table of compiled RTLs templates
    *
    *------------------------------------------------------------------------*)
   fun genRTLTable compiled_rtls =
   let val md   = RTLComp.md compiled_rtls

       val rtls = RTLComp.rtls compiled_rtls

       val rtlStrName = Comp.strname md "RTL" 

       val constTbl = Consts.newConstTable()

       fun makeEntry(RTLComp.RTLDEF{id, args, rtl, ...}) =  
       let val lookup = RTL.argOf rtl

           fun param i = APPexp(IDexp(IDENT(["T"],"PARAM")),INTexp i)

           fun makeArg name =
           let val (exp,pos) = lookup name
               val e =
                   case pos of
                     RTL.IN i    => param i
                   | RTL.OUT i   => param i
                   | RTL.IO(i,_) => param i
           in  (name, e)
           end handle RTL.NotAnArgument =>
               (warning("'"^name^"' is unused in rtl "^id);
                (name,param 0)
               )

           val arg = Consts.const constTbl (RECORDexp(map makeArg args))
       in  VALdecl[VALbind(IDpat id,
                    APPexp(makeNewRTL,
                           APPexp(IDexp(IDENT([rtlStrName],id)),
                           arg)))
                  ]
       end

       val body = map makeEntry rtls  

   in  STRUCTUREdecl("Arch",[],
                     NONE,DECLsexp
                       [LOCALdecl(Comp.Consts.genConsts constTbl,body)
                       ])
   end

   (*------------------------------------------------------------------------
    *
    * Create the function rtl : instruction -> rtl
    *
    *------------------------------------------------------------------------*)
   fun mkRtlQueryFun compiled_rtls =
   let fun body{instr, rtl=RTLComp.RTLDEF{id,...}, const} = 
           {exp=IDexp(IDENT(["Arch"],id)), casePats=[]}
   in  RTLComp.mkQuery compiled_rtls
          {name          = "rtl",
           namedArguments= true,
           args          = [["instr"]], 
           decls         = [RTLComp.complexErrorHandler "rtl"],
           caseArgs      = [],
           body          = body
          }
   end

   (*------------------------------------------------------------------------
    *
    * Create the function defUse : instruction -> cell list * cell list
    *
    *------------------------------------------------------------------------*)
   fun mkDefUseQueryFun compiled_rtls name =
   let val {get, decl} = M.getOpnd
            [("int",     M.CONV("CELL(int x)")),
             ("int32",   M.CONV("CELL(int32 x)")),
             ("intinf",  M.CONV("CELL(intinf x)")),
             ("word",    M.CONV("CELL(word x)")),
             ("word32",  M.CONV("CELL(word32 x)")),
             ("cell",    M.CONV("CELL x")),
             ("label",   M.IGNORE),
             ("cellset", M.MULTI("map CELL (CellsBasis.CellSet.toCellList x)")),
             ("operand", M.CONV("OPERAND x"))
            ]
        val decl0 =
            $["(* methods for computing value numbers *)",
              "val OT.VALUE_NUMBERING",
              "   {int, int32, intinf, word, word32, operand, ...} =",
              "      valueNumberingMethods",
              "(* methods for type conversion *)"
             ]       
       fun gen x = SOME(get x)
   in  RTLComp.mkDefUseQuery 
          compiled_rtls
          {name           = name,
           args           = [["valueNumberingMethods"], ["instr"]],
           namedArguments = false,
           decls          = [RTLComp.complexErrorHandler name, decl0, decl],
           def            = gen,
           use            = gen
          }
   end

   (*------------------------------------------------------------------------
    *
    * Main routine
    *
    *------------------------------------------------------------------------*)
   fun gen compiled_rtls =
   let (* The machine description *)
       val md = RTLComp.md compiled_rtls

       (* name of the structure/signature *)
       val strName = Comp.strname md "RTLProps"  
       val sigName = "RTL_PROPERTIES"
 
       (* Arguments to the instruction functor *)
       val args =
           ["structure Instr : "^Comp.signame md "INSTR",
            "structure RegionProps : REGION_PROPERTIES",
            "structure RTL : MLTREE_RTL",
            "structure OperandTable : OPERAND_TABLE where I = Instr",
            "structure Asm : INSTRUCTION_EMITTER where I = Instr",
            "  sharing Instr.T = RTL.T"
           ]

       (* The functor *)
       val strBody = 
           [$ ["structure I   = Instr",
               "structure C   = I.C",
               "structure RTL = RTL",
               "structure T   = RTL.T",
               "structure OT  = OperandTable",
               "",
               "datatype value = CELL of C.cell",
               "               | OPERAND of I.operand",
               ""
              ],
            Comp.errorHandler md "RTLProps",
            RTLComp.complexErrorHandlerDef (),
            STRUCTUREdecl(Comp.strname md "RTL",[],NONE,
               APPsexp(IDsexp(IDENT([],Comp.strname md "RTL")),
                  DECLsexp[
                  $[ "structure RTL = RTL",
                     "structure C   = C"
                   ]]
                  )
            ),
            genRTLTable compiled_rtls,
            mkRtlQueryFun compiled_rtls,
            mkDefUseQueryFun compiled_rtls "defUse" 
           ]

   in  Comp.codegen md "mltree/RTLProps"
         [Comp.mkFct md "RTLProps" args sigName
             (map Tr.simplifyDecl strBody)
         ]
   end
end
