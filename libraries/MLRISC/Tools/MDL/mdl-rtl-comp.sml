(*
 * Process rtl descriptions
 *)
functor MDLRTLComp
   (structure Typing      : MDL_TYPING
    structure RTLTools    : MDL_RTL_TOOLS
    structure MLRiscTypes : MLRISC_TYPES
      sharing Typing.Ast = RTLTools.Ast = MLRiscTypes.Ast 
      sharing MLRiscTypes.RTL = RTLTools.RTL
   ) : MDL_RTL_COMP =
struct
   structure Comp        = Typing.Comp
   structure Ast         = Comp.Ast
   structure AstPP       = Comp.AstPP
   structure Env         = Comp.Env
   structure Consts      = Comp.Consts
   structure Tr          = Comp.Trans
   structure R           = Comp.Rewriter
   structure H           = HashTable
   structure TypeUtil    = Typing.TypeUtil
   structure MLRiscTypes = MLRiscTypes
   structure RTL         = RTLTools.RTL
   structure T           = RTL.T
   structure C           = CellsBasis

   open Ast Comp.Util Comp.Error

   val t2s = PP.text o AstPP.ty 
   val e2s = PP.text o AstPP.exp 
   val p2s = PP.text o AstPP.pat 
   val d2s = PP.text o AstPP.decl
   val re2s = RTL.Util.rexpToString 

   val rw  = R.rewrite
   val NIL = R.noRewrite       
   val i2s = Int.toString

   fun tuplepat [p] = p
     | tuplepat ps  = TUPLEpat ps
   fun tupleexp [e] = e
     | tupleexp es  = TUPLEexp es


   exception NoRTL

   datatype rtl_def = 
      RTLDEF of {id   : Ast.id, 
                 args : Ast.id list, 
                 rtl  : RTL.rtl
                }
                   

   datatype compiled_rtls = COMPILED_RTLs of 
     { md       : Comp.md,
       env      : Env.env,
       rtls     : rtl_def list,
       newOps   : T.Basis.misc_op list,
       rtlTable : (string,rtl_def) H.hash_table
     }

   val current_rtls = ref [] : rtl_def list ref

   val makeRTLDEF = IDexp(IDENT(["MDLRTLComp"],"RTLDEF"))
   fun md(COMPILED_RTLs{md, ...}) = md
   fun rtls(COMPILED_RTLs{rtls, ...}) = rtls

   fun noError() = !errorCount = 0

   (*------------------------------------------------------------------------  
    *
    * Perform type interference and arity raising
    *
    *------------------------------------------------------------------------*)
   fun typeInference(md, rtlDecls) = 
   let (* Perform typechecking + arity raising *)
       val (semantics, env) = 
           (print "Typechecking...\n";
            Typing.typeCheck md rtlDecls)

       (* Make sure that there are 
        * no unresolved type applications after
        * arity raising.
        *)
       fun checkSemantics semantics =
       let fun checkUnresolvedTypeApplications(d,loc) = 
           let val poly = ref false
               fun exp ==> (e as TYPEexp t) =
                   (if Typing.isPolymorphic t then poly := true else (); e)
                 | exp ==> e = e
           in  #decl (rw{exp=exp,ty=NIL,decl=NIL,sexp=NIL,pat=NIL}) d;
               if !poly then
                 errorPos(loc,"unresolved polytype application in:\n"^d2s d)
               else ()
           end

            fun decl ==> d =
            (case d of 
              MARKdecl(l,d as VALdecl _) => 
                 checkUnresolvedTypeApplications(d, l)
            | RTLdecl(_,_,loc) => checkUnresolvedTypeApplications(d, loc)
            | _ => ();
            d 
            )
       in  #decl (rw{exp=NIL,ty=NIL,decl=decl,sexp=NIL,pat=NIL}) semantics;
           ()
       end

   in  if noError() then checkSemantics semantics else ();
       (semantics, env) 
   end

   (*------------------------------------------------------------------------
    * Translate the rtl declarations into an executable form.
    *------------------------------------------------------------------------*)
   fun codeGen(md, env, rtlDecls) = 
   let fun cellOf k = 
           let val CELLdecl{id, bits, ...} = Comp.lookupCellKind md k
           in  TUPLEexp[IDexp(IDENT(["C"],id)),INTexp bits] 
           end

       fun exp ==> (LOCexp(m,e,NONE)) = APPexp(APP("$",cellOf m),e)
         | exp ==> (LOCexp(m,e,SOME r)) = 
              APPexp(APP("Mem",cellOf m),TUPLEexp[e,ID r])
         | exp ==> (IFexp(a,b,c))   = APP("If",TUPLEexp[a,b,c])
         | exp ==> (TUPLEexp [])    = ID "Nop"
         | exp ==> (IDexp(IDENT([],"="))) = ID "=="
         | exp ==> (TYPEDexp(e,_)) = e
         | exp ==> (APPexp(BITSLICEexp(e,r),t)) = 
              APPexp(APPexp(APP("BitSlice",t),
                 LISTexp(map (fn (a,b) => TUPLEexp[INTexp a,INTexp b]) r,
                         NONE)),e)
         | exp ==> (LITexp(BOOLlit false)) = ID "False"
         | exp ==> (LITexp(BOOLlit true)) = ID "True"
         | exp ==> (IDexp(IDENT([],"not"))) = ID "Not"
         | exp ==> (IDexp(IDENT([],"andalso"))) = ID "And"
         | exp ==> (IDexp(IDENT([],"cond"))) = ID "Cond"
         | exp ==> (IDexp(IDENT([],"orelse"))) = ID "Or"
         | exp ==> (IDexp(IDENT([],"||"))) = ID "Par"
         | exp ==> e = e

       (* All rtl definitions *)
       val allRtls = ref []

       fun addRtls(p, loc) = 
       let fun processBinding x =
           let val (_,t) = Env.lookupVal env (IDENT([],x))
               val t = #ty (rw{exp=NIL,pat=NIL,decl=NIL,sexp=NIL,ty=NIL}) t
           in  if Typing.isPolymorphic t then
                    errorPos(loc, "rtl "^x^" has polymorphic type "^
                             t2s t) 
               else 
               case t of 
                  FUNty(RECORDty lts,_) => (allRtls := (x,lts,loc) :: !allRtls)
                | t => errorPos(loc, 
                          "rtl "^x^" has a non-function type "^t2s t) 
           end
           fun pat ==> (p as IDpat x) = (processBinding x; p)
             | pat ==> p = p
       in #pat (rw{exp=NIL,ty=NIL,decl=NIL,sexp=NIL,pat=pat}) p end

       fun decl ==> (DATATYPEdecl _) = SEQdecl[]
         | decl ==> (TYPESIGdecl _) = SEQdecl[]
         | decl ==> (VALSIGdecl _) = SEQdecl[]
         | decl ==> (VALdecl[VALbind(LISTpat(pats,NONE),
                       APPexp(
                          APPexp(APPexp(IDexp(IDENT([],"map")),_),f),
                              LISTexp(es,NONE)))]) =
              VALdecl(ListPair.map (fn (p,e) => VALbind(p,APPexp(f,e)))
                         (pats,es))
         | decl ==> (VALdecl[VALbind(LISTpat(pats,NONE),LISTexp(es,NONE))]) =
              VALdecl(ListPair.map VALbind (pats,es))
         | decl ==> (RTLdecl(pat,exp,loc)) = 
              (addRtls(pat,loc); ==>(VALdecl[VALbind(pat,exp)]))
         | decl ==> (MARKdecl(_,SEQdecl [])) = SEQdecl[]
         | decl ==> d = d

      (* Define the cellkinds in a substructure C *)
      val cellKindDecls =
           VALdecl(map (fn CELLdecl{id, nickname, ...} =>
                     VALbind(IDpat id, 
                        APPexp(
                           IDexp(IDENT(["C"],"newCellKind")),
                           RECORDexp[("name",STRINGexp id),
                                     ("nickname",STRINGexp nickname)
                                    ])))
                        (Comp.cells md))
       
      val userRtlDecls = 
             #decl (rw{exp=exp,pat=NIL,sexp=NIL,decl=decl,ty=NIL}) rtlDecls
      val allDecls = SEQdecl[STRUCTUREdecl("C",[],NONE,
                                 DECLsexp[cellKindDecls]), 
                             userRtlDecls]
   in (allDecls, rev(!allRtls)) 
   end

   (*------------------------------------------------------------------------
    * Rewrite the program to fill in all syntactic shorthands
    *------------------------------------------------------------------------*)
   fun expandSyntacticSugar(md, rtlDecls) =
   let (* Function to define a new operator *)
       fun newRtlOp argTy f =
           let fun newVars(i,n) =
               if i < n then ("x"^i2s i)::newVars(i+1,n)
               else []
               fun arity(TUPLEty x) = length x
                 | arity _ = 1
               val names = newVars(0,arity argTy)
               val formals = TUPLEpat(map IDpat names)
               val actuals = LISTexp(map ID names,NONE)
           in  LOCALdecl([VAL("newOper",APP("newOp",STRINGexp f))],
                         [FUN(f,formals,APP("newOper",actuals))])
           end

       (* Rewrite the program first to fill in all syntactic shorthands *)
       fun exp ==> (e as LITexp(INTlit _))  = APP("intConst", e)
         | exp ==> (e as LITexp(WORD32lit _)) = APP("wordConst",e)
         | exp ==> (e as LITexp(WORDlit _)) = APP("wordConst",e)
         | exp ==> e = e
       fun decl ==> (RTLSIGdecl(fs,FUNty(argTy,_))) =
             SEQdecl(map (newRtlOp argTy) fs)
         | decl ==> (d as RTLSIGdecl(fs,ty)) = (error("bad type in "^d2s d); d)
         | decl ==> d = d

       val rtlDecls = 
            #decl (rw{exp=exp,pat=NIL,decl=decl,sexp=NIL,ty=NIL}) rtlDecls

   in  rtlDecls 
   end

   (*------------------------------------------------------------------------
    * Compile a file.
    * Turn off pattern matching warnings
    *------------------------------------------------------------------------*)
   fun compileFile filename =
   let val warn     = Control.MC.bindNonExhaustiveWarn 
       val previous = !warn
       fun reset() = warn := previous 
   in  warn := false;
       (Backend.Interact.useFile filename; reset()) 
         handle e => (reset(); raise e)
   end

   (*------------------------------------------------------------------------
    * Process the rtl description 
    ------------------------------------------------------------------------*)
   fun compile md =
   let (* The semantics environment *)
       val semantics = Comp.declOf md "RTL"

       (* Expand Syntactic sugar *)
       val semantics = expandSyntacticSugar(md, semantics)

       (* Perform typechecking *)
       val (semantics, env) = typeInference(md, semantics)

       (* Generate the rtl functions defined by the user *)
       val (userRtlDecls, allRtls) = codeGen(md, env, semantics)

       (* Generate the rtl table *)
       val rtlTable = 
       if !errorCount = 0 then
       let fun mkEntry (name,args,loc) =
           let fun mkArg(arg,ty) =
               let val (sz,kind) =
                       MLRiscTypes.representationOf(name, arg, loc, ty)
               in  (arg,APP("Arg",
                          TUPLEexp[INTexp sz,STRINGexp kind,STRINGexp arg])
                   )
               end

           in  APPexp(makeRTLDEF,
                  RECORDexp
                  [("id",STRINGexp name),
                   ("args",
                     LISTexp(map (fn (x,_) => STRINGexp x) args,NONE)),
                   ("rtl",APP(name, RECORDexp(map mkArg args)))
                  ]
               )
           end
       in  VALdecl[VALbind(IDpat "rtls", LISTexp(map mkEntry allRtls,NONE))]
       end else $[]

       val strname = Comp.strname md "RTL"

       (* Now generate the code that MDGen uses *)
       val code =
         LOCALdecl(
             [STRUCTUREdecl(strname,[$["Build : RTL_BUILD"]],NONE,
                 DECLsexp
                   [LOCALdecl([OPENdecl [IDENT([],"Build")],
                               $["structure C = CellsBasis"]
                              ],
                              [userRtlDecls])]),
              STRUCTUREdecl(strname,[],NONE,
                            APPsexp(IDsexp(IDENT([],strname)),
                                    IDsexp(IDENT([],"MDLRTLBuilder")))),
              LOCALdecl([OPENdecl [IDENT([],"MDLRTLBuilder"),
                                   IDENT([],strname)]],
                        [rtlTable])
             ],
             [ 
               $["val _ = MDLRTLComp.current_rtls := rtls"]
             ]
         )

       (* Compile RTL into internal form *)
       fun elaborateRTL(code) = 
       if !errorCount = 0 then 
       let val _    = current_rtls := [] 
           val name = "CompileRTL"
           val _    = print "Generating ML code for computing RTLs...\n";
           val _ = Comp.codegen md name [AstPP.decl code]
           val filename = Comp.pathName md name ".sml"
       in  print "Calling the ML compiler to build the rtls ...\n";
           print "This may take a while...\n";
           compileFile filename
       end
       else ()

 
       (* Execute the code *)
       val _      = elaborateRTL(code)
       val newOps = MDLRTLBuilder.getNewOps()
       val _      = MDLRTLBuilder.clearNewOps()

       (* Build a table of rtls *)
       val rtlTable = H.mkTable(HashString.hashString,op=) (32,NoRTL)
       val allRtls  = !current_rtls
       val _        = 
           app (fn def as RTLDEF{id,...} => H.insert rtlTable (id,def)) allRtls

   in  COMPILED_RTLs{md       = md,
                     env      = env,
                     rtls     = allRtls,
                     newOps   = newOps,
                     rtlTable = rtlTable
                    }
   end

   (*------------------------------------------------------------------------
    * Pretty print RTL code 
    *------------------------------------------------------------------------*)
   fun dumpLog(COMPILED_RTLs{md, rtls, newOps, ...}) = 
   let fun prNewOp{name, hash, attribs} = 
           "New abstract operator "^name^"\n" 

       fun prRTL(def as RTLDEF{id=f, args, rtl, ...}) =
       let fun listify es = foldr (fn (x,"") => x | (x,y) => x^", "^y) "" es
           fun prs es = listify(map RTL.expToString es)
           fun prs' es = 
               listify(map (fn (e,r) => RTL.expToString e^"="^i2s r) es)
           val pretty = String.translate (fn #"\n" => "\n\t" 
                                           | #";"  => " ||"
                                           | c => Char.toString c)
           val (d, u) = RTL.defUse rtl
           val {fixedDefs, fixedUses, twoAddress} = RTL.namingConstraints(d,u)
           val rtlText = pretty(RTL.rtlToString rtl)
           val rtl = RTLTools.simplify rtl

           fun line(title,"") = ""
             | line(title,text) = "\t"^title^":\t"^text^"\n"   
       in  "rtl "^f^
                "{"^List.foldr(fn (x,"") => x | (x,y) => x^","^y) "" args^
                 "} =\n\t"^rtlText^"\n"^
           line("Define",prs d)^
           line("Use",prs u)^
           line("Pinned definitions",prs' fixedDefs)^
           line("Pinned uses",prs' fixedUses)^
           line("Two address operand",prs twoAddress)^
           line("Constructor",
                PP.text(AstPP.decl(RTLTools.rtlToFun(f, args, rtl))))^
           line("Destructor",
                PP.text(AstPP.pat(RTLTools.rtlToPat(rtl))))^
           "\n"
       end

       (* Sort them alphabetically *)
       val rtls = 
           ListMergeSort.sort 
           (fn (RTLDEF{id=f,...},RTLDEF{id=g,...}) => String.>(f,g)) rtls

       val nRTLs = length rtls
       val nNewOps = length newOps

       val text = 
           "There are a total of "::i2s nRTLs::" rtl templates defined.\n"::
           "There are a total of "::i2s nNewOps::" new abstract operators.\n"::
           "RTL information follows:\n\n"::
           map prNewOp newOps @ 
           ["\n\n"] @
           map prRTL rtls

   in  Comp.Error.printToLog (String.concat text)
   end

   (*------------------------------------------------------------------------
    * Gnerate code the ArchRTL functor 
    *------------------------------------------------------------------------*)
   fun genArchFunctor(COMPILED_RTLs{md, rtls, newOps, ...}) = 
   let (* The ArchRTL functor *)
       val strname = Comp.strname md "RTL"
    
       (* The main body are just the RTL constructor functions *)
       val decls = 
            $["structure T = RTL.T"
             ]::
             STRUCTUREdecl("P",[],NONE,
               DECLsexp(map RTLTools.createNewOp newOps))::
             map (fn RTLDEF{id,args,rtl} => RTLTools.rtlToFun(id,args,rtl)) 
                 rtls

       val archRTL = 
             STRUCTUREdecl(
                strname,
                [$["structure RTL : MLTREE_RTL",
                   "structure C   : "^Comp.signame md "CELLS"
                  ]
                ],
                NONE,
                DECLsexp decls
             )

       (* Write the functor to a file *)
       val _ = Comp.codegen md "mltree/RTL" [AstPP.decl archRTL]
   in  ()
   end


   (*------------------------------------------------------------------------
    *
    * Generic routine for generating query functions from rtl definitions.
    *
    *------------------------------------------------------------------------*)
    fun makeQuery warning (COMPILED_RTLs{rtls, md, rtlTable, ...}) =
    let (* The instructions *)
        val instructions = Comp.instructions md
 
        datatype rtlpat = LIT of string 
                        | TYP of string * datatypebind
   
        (* Lookup rtl *)
        fun lookupRTL name =
             H.lookup rtlTable name handle e =>
               (warning("Can't find definition for rtl "^name); raise e)

         (* error handler *)
        val errorHandler = APP("undefined",TUPLEexp []) 
        val errorHandlingClause = CLAUSE([WILDpat],NONE,errorHandler)

        fun mkQueryFun{namedArguments, name, args, body, caseArgs, decls} =
        let 
            val extraCaseArgs = map ID caseArgs

            (* Generate constants *)
            val constTbl = Consts.newConstTable()
            val mkConst  = Consts.const constTbl  

            (* Enumerate all rtl patterns and generate a case expression
             * that branch to different cases.
             *)
            fun foreachRtlPat genCode rtlpats =
                let fun enum([], pats, name) = [(pats, name)]
                      | enum(LIT s::rest,pats,name) = enum(rest,pats,s^name)
                      | enum(TYP(_,DATATYPEbind{cbs, ...})::rest,pats,name) =
                        let val names =
                            map (fn cb as CONSbind{id, ...} => 
                                 let val pat = 
                                     Tr.mapConsToPat
                                        {prefix=["I"],
                                         id=fn{newName,...}=>IDpat newName
                                        } cb
                                 in  enum(rest, pat::pats, id^name)
                                 end) cbs
                        in  List.concat names end
                    fun caseExps [] = []
                      | caseExps (LIT _::rest) = caseExps rest
                      | caseExps (TYP(x,_)::rest) = ID x::caseExps rest
                    val exps  = caseExps rtlpats
                    val cases = enum(rev rtlpats, [], "")
                    val clauses = map genCode cases
                in  CASEexp(tupleexp(exps @ extraCaseArgs), clauses)
                end

            (* Enumerate each instruction *)
            and doInstr (CONSbind{rtl=NONE, ...}) = raise NoRTL
              | doInstr (instr as CONSbind{rtl=SOME rtlDef,id,loc, ...})=
                let val _ = setLoc loc
                    val E = Tr.consBindings instr (* bindings for the instr *)

                    (* Translate rtl definition *)
                    fun trans(TEXTasm s) = LIT s
                      | trans(EXPasm(IDexp(IDENT([],x)))) = 
                        let val (_, ty) = E x handle _ =>
                                  fail("unknown identifier "^x^
                                       " in rtl expression: "^e2s rtlDef)
                            val db = 
                                case ty of   
                                  IDty(IDENT([],t)) => Comp.lookupDatatype md t
                                | t => fail("illegal type "^t2s t)
                        in  TYP(x,db) end
                      | trans(EXPasm e) = fail("illegal rtl expression "^e2s e)
    
                    fun exp _ (e as RTLexp [COMPOSITErtl _]) = e
                      | exp _ (ASMexp(ASMasm rtl)) = 
                          foreachRtlPat (genCode(instr, E)) (map trans rtl)
                    val rw = rw{exp=exp,decl=NIL,pat=NIL,ty=NIL,sexp=NIL}
                in  #exp rw rtlDef 
                end  

                (* Call the user defined callback and generate code *)
            and genCode (instr, E) (pats, rtlName) =
                let val rtl as RTLDEF{args,...} = lookupRTL rtlName
                    val {casePats,exp} = 
                         body{const=mkConst,rtl=rtl,instr=instr}
                    fun simpList(ps) = 
                    let fun loop [] = []
                          | loop (WILDpat::ps) =
                              (case loop ps of
                                [] => []
                              | ps => WILDpat::ps
                              )
                          | loop (p::ps) = p::loop ps
                    in  case loop ps of
                          [] => WILDpat
                        | ps => LISTpat(ps,SOME WILDpat)
                    end
                    fun simplifyPat(LISTpat(ps,NONE)) = simpList ps
                      | simplifyPat(LISTpat(ps,SOME WILDpat)) = simpList ps
                      | simplifyPat(TUPLEpat[p]) = simplifyPat p
                      | simplifyPat pat = pat
                    val casePats = map simplifyPat casePats
                in  CLAUSE([tuplepat(pats@casePats)],NONE,exp)
                end handle _ => errorHandlingClause


            datatype err = OK | BAD

            (* process all instructions *)
            fun foreachInstr([], OK) = []
              | foreachInstr([], BAD) = [errorHandlingClause]
              | foreachInstr(instr::instrs, err) =
                Tr.mapConsToClause{prefix=["I"],
                                   pat=fn pat => pat,
                                   exp=doInstr instr
                                  } instr::
                         foreachInstr(instrs, err)      
                handle _ => foreachInstr(instrs, BAD) 

            val clauses = foreachInstr(instructions, OK) 

            val queryFun = FUNdecl[FUNbind("query", clauses)] 

           (* How to make an argument:  
            * If the argument has more than one
            * name we'll first pack them into a record pattern. 
            *)
           fun mkArg [x] = IDpat x
             | mkArg xs  = 
                if namedArguments then
                   RECORDpat(map (fn x => (x,IDpat x)) xs,false)
                else
                   TUPLEpat(map IDpat xs)

            val wrapper = 
                [FUNdecl[FUNbind(name, 
                     [CLAUSE(map mkArg args,  
                             NONE,
                             LETexp(decls @ [queryFun], 
                                    [APP("query",ID "instr")]))
                     ])
                  ]
                ]

            val constants = Consts.genConsts constTbl 
        in  Tr.simplifyDecl
            (case constants of
               [] => SEQdecl wrapper
            |  _  => LOCALdecl(constants, wrapper)
            )
        end

   in   mkQueryFun
   end

   val mkQuery = makeQuery (fn _ => ())

   (*------------------------------------------------------------------------
    *
    * Generic routine that enumerates all arguments in an 
    * instruction constructor.
    *
    *------------------------------------------------------------------------*)
   fun forallArgs{instr, rtl=RTLDEF{rtl, ...}, rtlArg, nonRtlArg} unit =
   let val lookupArg = RTL.argOf rtl
       fun every({origName,newName,ty},x) =  
           let val (exp, pos) = lookupArg newName 
           in  rtlArg(newName, ty, exp, pos, x)
           end handle RTL.NotAnArgument => nonRtlArg(newName, ty, x)
   in  Tr.foldCons every unit instr
   end

   (*------------------------------------------------------------------------
    *
    * Generic routine for generating a query function on the operand type 
    *
    *------------------------------------------------------------------------*)
   fun mkOperandQuery compiled_rtls = 
   let val md = md compiled_rtls
   in  ()
   end


   (*------------------------------------------------------------------------
    *
    * Generic routine that maps an instruction
    *
    *------------------------------------------------------------------------*)
   fun mapInstr{instr, rtl=RTLDEF{rtl, ...}, rtlArg, nonRtlArg} =
   let val lookupArg = RTL.argOf rtl
       val changed = ref false
       fun mapArg{origName,newName,ty} =  
           let val (exp, pos) = lookupArg newName 
           in  case rtlArg(newName, ty, exp, pos) of
                 SOME e => (changed := true; e)
               | NONE   => ID newName
           end handle RTL.NotAnArgument => 
               (case nonRtlArg(newName, ty) of
                 SOME e => (changed := true; e)
               | NONE   => ID newName
               )
       val exp = Tr.mapConsToExp {prefix=["I"], id=mapArg} instr
   in  if !changed then exp else ID "instr"
   end

   (*------------------------------------------------------------------------
    *
    * Generate RTL code for def/use like queries
    *
    *------------------------------------------------------------------------*)
   fun mkDefUseQuery compiled_rtls 
        { name, decls, def, use, namedArguments, args } = 
   let val md = md compiled_rtls
       val trivial = ref true
       val Nil = LISTexp([], NONE)

       fun defUseBody{instr, rtl=RTLDEF{rtl, ...}, const} = 
       let val bindings =
               Tr.foldCons (fn({newName,ty,...},L) => (newName,ty)::L) [] instr
           fun lookup id = List.find (fn (x,_) => x=id) bindings
           fun add(f, x, e, y) =     
                case f(x, e, y) of
                  SOME e => e
                | NONE => y
               
           fun fold f (e as T.ARG(_,_,x),exp) = add(f, ID x, e, exp)
             | fold f (e as T.$(_,_,T.ARG(_,_,x)),exp) = add(f, ID x,e,exp)
             | fold f (e as T.$(_,k,T.LI i), exp) =
               let val CELLdecl{id, ...} = 
                         Comp.lookupCellKind md (C.cellkindToString k)
                   val cell = 
                          APPexp(APPexp(IDexp(IDENT(["C"],"Reg")),
                                        IDexp(IDENT(["C"],id))),
                                      INTexp(IntInf.toInt i))
               in  add(f,const cell,e,exp)
               end
             | fold f (_, exp) = exp

           val (d, u) = RTL.defUse rtl
           val d = List.foldr (fold def) Nil d
           val u = List.foldr (fold use) Nil u
       in  case (d, u) of
             (LISTexp([], NONE), LISTexp([], NONE)) => ()
           | _ => trivial := false;
           {exp=TUPLEexp[d, u],
            casePats=[]
           }
       end
       val decl = 
         mkQuery compiled_rtls
          {name=name, namedArguments=namedArguments, args=args, decls=decls,
           caseArgs=[], body=defUseBody
          } 
   in  if !trivial then FUN(name,WILDpat,TUPLEexp[Nil,Nil])
       else decl
   end

   (*------------------------------------------------------------------------
    *
    * Make a simple error handler
    *
    *------------------------------------------------------------------------*)
   fun simpleErrorHandler name =
       $["fun undefined() = error \""^name^"\""]

   (*------------------------------------------------------------------------
    *
    * Make a complex error handler
    *
    *------------------------------------------------------------------------*)
   fun complexErrorHandler name =
       $["fun undefined() = bug(\""^name^"\",instr)"]

   (*------------------------------------------------------------------------
    *
    * Make a complex error handler
    *
    *------------------------------------------------------------------------*)
   fun complexErrorHandlerDef() =
       $["fun bug(msg,instr) =",
         "let val Asm.S.STREAM{emit, ...} = Asm.makeStream []",
         "in  emit instr; error msg end"
        ]

   (*------------------------------------------------------------------------
    *
    * Do consistency checking on the RTL and instruction representation.
    * Call mkQuery to test the entire process.  
    *
    *------------------------------------------------------------------------*)
   fun consistencyCheck compiled_rtls =
   let val md = md compiled_rtls

       (* Check one instruction *)
       fun check{instr as CONSbind{id=instrName,...},
                 rtl=RTLDEF{id=f,args,rtl,...},const} = 
       let (* Find all arguments in the instruction constructor *)
           val bindings =
               Tr.foldCons 
                  (fn({newName,ty,...},L) => 
                     (newName,ref false,ty)::L) [] instr

           fun lookup id = List.find (fn (x,_,_) => x=id) bindings
           val lookupRTLArg = RTL.argOf rtl

           fun checkIt(x,exp,pos,ty) =
           let fun err(why) =
                (error("in instruction "^instrName^" (rtl "^f^"):");     
                 if why = "" then () else log(why);
                 log("rtl argument "^re2s exp^
                     " cannot be represented as "^t2s ty)
                )
           in  MLRiscTypes.insertRepCoercion(exp,ty);
               case (exp,ty) of
                 (T.$(_,k,T.ARG _),CELLty cellkind) => 
                 let val CELLdecl{id, ...} = 
                      Comp.lookupCellKind md cellkind 
                 in  if C.cellkindToString k = id then ()
                     else err("cellkind mismatched")
                 end
               | (exp, CELLty _) => err("rtl is not a register reference")
               | (T.$(_,_,T.ARG _),ty) => err ""
               | (T.ARG(ty,ref(T.REP k),_),IDty(IDENT(_,typeName))) => 
                   if k = typeName then ()
                   else err("representation mismatch") 
               | (_, _) => err("")
           end handle _ => ()

          (* Check one argument in rtl *)
           fun checkRTLArg x =
           let val (exp,pos) = lookupRTLArg x
           in  case lookup x of
                 SOME(_,found,ty) => (found := true; checkIt(x,exp,pos,ty))
               | NONE => error("'"^x^"' of rtl "^f^
                               " is missing from instruction "^instrName)
           end

          (* Check one argument in instruction *)
           fun checkInstrArg(name,ref true,ty) = ()
             | checkInstrArg(name,ref false,ty) =
               if MLRiscTypes.isSpecialRepType ty then
                  warning("In instruction "^instrName^" (rtl "^f^"): '"^
                          name^"' has type "^
                          t2s ty^" but its meaning is unspecified in the rtl"
                         )
               else ()

       in  app checkRTLArg args;
           app checkInstrArg bindings;
           {casePats=[], exp=TUPLEexp []} 
       end
       val _ = print "Consistency checking...\n"
       val _ = makeQuery warning compiled_rtls 
                  {name="check",namedArguments=false,
                   args=[],decls=[],caseArgs=[], body=check}
   in  ()
   end

   (*------------------------------------------------------------------------
    *
    * Generate RTL code and write the log
    *
    *------------------------------------------------------------------------*)
   fun gen compiled_rtls =
      (genArchFunctor compiled_rtls;
       consistencyCheck compiled_rtls
      )
end
