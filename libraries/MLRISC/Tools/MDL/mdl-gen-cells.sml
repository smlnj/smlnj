(* mdl-gen-cells.sml
 *
 * COPYRIGHT (c) 2016 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Generate the <arch>Cells structure.
 * This structure contains various information about the register
 * properties of the architecture.
 *)

functor MDLGenCells(Comp : MDL_COMPILE) : MDL_GEN_MODULE =
struct

   structure Ast  = Comp.Ast
   structure Comp = Comp
   structure R    = Comp.Rewriter

   open Ast Comp.Util

   val NO = R.noRewrite

   val SZty          = IDty(IDENT([],"CellsBasis.sz"))
   val REGISTER_IDty = IDty(IDENT([],"CellsBasis.register_id"))
   val showFunTy  = FUNty(REGISTER_IDty, STRINGty)
   val showWithSizeFunTy = FUNty(TUPLEty[REGISTER_IDty,SZty], STRINGty)

   fun gen md =
   let (* name of the structure *)
       val strName = Comp.strname md "Cells"  
       val sigName = Comp.signame md "CELLS"

       (* all cell kinds *)
       val cellKinds = Comp.cells md

       (* Process *) 
       fun process([], r) = r
         | process(CELLdecl{id, from, to, count, ...}::ds, r) =
           let val count = case count of NONE => 0 | SOME c => c
           in  from := r;
               to   := r + count - 1;
               process(ds, r+count)
           end
  
       val firstPseudo = process(cellKinds, 0)
            

       (* all cell kind names *)
       val cellKindNames = map (fn CELLdecl{id, ...} => id) cellKinds

       val allCellKindNames = cellKindNames 

       (* cellkinds that has to be put into the cellset *)
       (* val cellSets = Comp.cellSets md
          val cellSets' = Comp.cellSetsAliases md

       val cellSetNames = map (fn CELLdecl{id, ...} => id) cellSets
        *)

       val clientDefinedCellKinds = 
           List.filter (fn CELLdecl{id, ...} => 
              not(MLRiscDefs.isPredefinedCellKind id)) cellKinds

       (* locations *)
       val locations = Comp.locations md


       (* Functions showXXX *)
       val showFunSig = 
            VALSIGdecl(map (fn k => "show"^k) cellKindNames,showFunTy)
       val showWithSizeFunSig = 
            VALSIGdecl(map (fn k => "show"^k^"WithSize") 
                       cellKindNames,showWithSizeFunTy)
       val showWithSizeFuns =
           let fun shift(from, to) e = 
                   if !from = 0 then e
                   else LET([VAL("r",
                           IFexp(APP("<=",TUPLEexp[ID "r",INTexp(!to)]),
                                 APP("-",TUPLEexp[ID "r",INTexp(!from)]),
                                 ID "r"))],e)
           in  FUNdecl(
                map (fn CELLdecl{id, from, to, print, ...} =>
                 FUNbind("show"^id^"WithSize",
                  [CLAUSE([TUPLEpat[IDpat "r",IDpat "ty"]],
                          NONE,    
                           (APPexp(print, TUPLEexp[ID "r",ID "ty"])))]))
                    cellKinds)
           end

       val showFuns = 
           SEQdecl(map (fn CELLdecl{id, from, to, print, bits, ...} =>
               FUN("show"^id,IDpat "r",
                       APP("show"^id^"WithSize",TUPLEexp[ID "r",INTexp bits])))
                          cellKinds)
   
       (* Functions addXXX *)
       val addFunTy = FUNty(TUPLEty[REGISTERty,CELLSETty],CELLSETty)
       val addFunSig = VALSIGdecl(map (fn s => "add"^s) 
                                  cellKindNames, addFunTy)
       val addFun  = VALdecl
             (map (fn k => VALbind(IDpat("add"^k),
                               IDexp(IDENT(["CellSet"],"add"))))
                             cellKindNames)

       (* Client defined cellkinds *)
       val clientDefinedCellKindsSig = 
           VALSIGdecl(map (fn CELLdecl{id, ...} => id) clientDefinedCellKinds,
                      IDty(IDENT([],"CellsBasis.cellkind")))
       fun createCellKind(CELLdecl{id, nickname, ...}) =
             VALbind(IDpat id, 
                APPexp(IDexp(IDENT(["CellsBasis"],"newCellKind")),
                       RECORDexp[("name",STRINGexp id), 
                                 ("nickname",STRINGexp nickname)]))
           
       val clientDefinedCellKindsDecl = 
           VALdecl(map createCellKind clientDefinedCellKinds)

       val None = ID "NONE" 
       val newCounter = APP("ref",INTexp 0)


       val nonAliasedCellKinds = 
           List.filter(fn CELLdecl{alias=NONE, ...} => true
                        | CELLdecl _ => false) cellKinds

       fun kindName k = 
           if MLRiscDefs.isPredefinedCellKind k
           then IDexp(IDENT(["CellsBasis"],k))
           else ID k

 
       (* Generate descriptor for a cellkind *)
       fun mkDesc(CELLdecl{from, to, id, nickname, defaults, ...}) = 
       let val zeroReg = 
              List.foldr(fn ((r,LITexp(INTlit 0)),_) => APP("SOME",INTexp r)
                          | (_,d) => d) None defaults
           val defaultValues = 
               LISTexp(map (fn (r,v) => TUPLEexp[INTexp(r + !from),v])
                           defaults, NONE) 

           val count = Int.max(!to - !from + 1,0)
           val physicalRegs = APP("ref", ID("CellsBasis.array0"))
           val exp = 
             APP("CellsBasis.DESC",
               RECORDexp[("low",           INTexp(!from)),
                         ("high",          INTexp(!to)),
                         ("kind",          kindName id),
                         ("defaultValues", defaultValues),
                         ("zeroReg",       zeroReg),     
                         ("toString",       ID("show"^id)),
                         ("toStringWithSize", ID("show"^id^"WithSize")),
                         ("counter",          newCounter),
			 ("dedicated",	      newCounter),
                         ("physicalRegs",     physicalRegs)
                        ]
              )
       in  VALbind(IDpat("desc_"^id), exp) 
       end

       fun mkKindDesc(CELLdecl{alias=NONE, id, ...}) = 
               TUPLEexp[kindName id, ID("desc_"^id)]
         | mkKindDesc(CELLdecl{alias=SOME x, id, ...}) = 
               TUPLEexp[kindName id, ID("desc_"^x)]

       (* size of general-purpose registers in bytes *)
       val cellSize = let
	     val CELLdecl{bits=widthOfGP, ...} = Comp.lookupCellKind md "GP"
	     in
		widthOfGP div 8
	     end

       (* create CellsBasis *)
       val applyCellsCommon =
           STRUCTUREdecl("MyCells",[],NONE,
             APPsexp(IDsexp(IDENT([],"Cells")),
               DECLsexp
               [$["exception Cells = "^strName,
                  "val firstPseudo = 256"
                 ],
                VALdecl(map mkDesc nonAliasedCellKinds),
                VAL("cellKindDescs",LISTexp(map mkKindDesc cellKinds,NONE)),
		VAL("cellSize", LITexp(INTlit cellSize))
               ]))

       (* User defined locations *)
       (*
       val locationsSig = 
            map (fn LOCbind(id,NONE,_) => VALSIGdecl([id],REGISTERty)
                  | LOCbind(id,SOME _,_) =>
                     VALSIGdecl([id],FUNty(INTty,REGISTERty)))
                locations
       *)

       (* stackptrR, asmTmpR, and fasmTmpR are in the common CELLS
	* interface, so we do not include them in the architecture interface
	* as well -- or we would have a duplicate specification error.
	*)
       val locationsSig = let
	 fun locs(LOCbind("stackptrR",_,_)::rest) = locs rest
	   | locs(LOCbind("asmTmpR",_,_)::rest) = locs rest
	   | locs(LOCbind("fasmTmp",_,_)::rest) = locs rest
	   | locs(LOCbind(id,NONE,_)::rest) = VALSIGdecl([id],REGISTERty)::locs rest
	   | locs(LOCbind(id,SOME _,_)::rest) =
	       VALSIGdecl([id],FUNty(INTty,REGISTERty))::locs rest
	   | locs [] = []
       in
         locs locations
       end

       val locationsFun0 =
           VALdecl(map (fn CELLdecl{id, ...} =>
                        VALbind(IDpat("Reg"^id),APP("Reg",ID id)))
                          cellKinds)

       val locationsFun =
       let fun mkLoc e =
           let fun exp _ (LOCexp(id,e,_)) =
                   let val CELLdecl{id, ...} = Comp.lookupCellKind md id
                   in  APP("Reg"^id,e)
                   end 
                 | exp _ e = e
           in  #exp(R.rewrite{exp=exp,sexp=NO,decl=NO,ty=NO,pat=NO}) e
           end
       in
            map (fn LOCbind(id,NONE,e) => VAL(id,mkLoc e)
                  | LOCbind(id,SOME p,e) => 
                        VAL(id,LAMBDAexp[CLAUSE([p],NONE,mkLoc e)]))
                locations
       end

       fun set k = ID("set"^k)

       (* body of signature *) 
       val sigBody = 
          [$["include CELLS"],
           clientDefinedCellKindsSig,
           showFunSig,
           showWithSizeFunSig,
           SEQdecl locationsSig,
           addFunSig
          ]
                        
       (* body of structure *) 
       val strBody = 
           [$["exception "^strName,
              "fun error msg = MLRiscErrorMsg.error(\""^strName^"\",msg)",
	      "open CellsBasis"
             ],
            showWithSizeFuns,
            showFuns,
            clientDefinedCellKindsDecl,
            applyCellsCommon,
            $["open MyCells"],
            addFun,
            locationsFun0,
            SEQdecl locationsFun,
            Comp.declOf md "Cells"
           ] 
  
   in  
       Comp.codegen md "instructions/Cells" 
        [Comp.mkSig md "CELLS" sigBody,
         Comp.mkStr md "Cells" sigName strBody]

   end

end
