(*
 * Generate the <arch>SchedProps functor.
 * This structure extracts semantics and dependence 
 * information about the instruction set needed for scheduling optimizations.
 *)

functor MDLGenSchedProps(RTLComp : MDL_RTL_COMP) : MDL_GEN_MODULE2 =
struct

   structure RTLComp = RTLComp
   structure Comp    = RTLComp.Comp
   structure Ast     = Comp.Ast
   structure E       = Env
   structure Env     = Comp.Env
   structure RTL     = RTLComp.RTL
   structure T       = RTL.T

   open Ast Comp.Util Comp.Error

   exception Undefined
  
   (* Generate a call to the bug function *)
   fun bug funName = APP("bug",TUPLEexp[STRINGexp funName,ID "instr"])

   (* Expressions building utilities *)
   fun cons(x,LISTexp(a,b)) = LISTexp(x::a,b)
     | cons(x,y) = LISTexp([x],SOME y)
   fun append(x,LISTexp([],NONE)) = x 
     | append(x,y) = APP("@",TUPLEexp[x,y])

   val MAX_BIT = Word.wordSize - 1 (* Implementation specific!!!! *)

   (* Function to compile the reservation tables! *)

   fun genRT md = 
   let val resources   = Comp.resources md
       val cpus        = Comp.cpus md
       val pipelines   = Comp.pipelines md
       val latencies   = Comp.latencies md
       val arch        = Comp.name md
    
       type 'a table = 'a E.envir * string
       fun mkTbl name = (E.envir name, name)

       fun enter(E,name) (x,y) = 
           (E.lookup E x; error(name^" "^x^" has been redefined\n"))
           handle _ => E.update E (x,y)

       (* Create tables *)
       val resourceTbl = mkTbl "resource"
       val cpuTbl      = mkTbl "cpu"
       val pipelineTbl = mkTbl "pipeline"
       val latencyTbl  = mkTbl "latency"

       (* Enter info into tables *)
       val _ = app (fn id => enter resourceTbl (id,id) ) resources
       val _ = app (fn x as CPUbind{name,...} => enter cpuTbl (name,x)) cpus
       val _ = app (fn x as PIPELINEbind(id, _) => enter pipelineTbl (id,x)) 
                pipelines
       val _ = app (fn x as LATENCYbind(id, _) => enter latencyTbl (id,x)) 
                latencies

       fun checkResource msg r = 
            (E.lookup (#1 resourceTbl) r; ()) handle _ =>
              error(msg^" has undefined resource "^r)
 
       (* Compile each CPU definition *)
       fun compileCPU(CPUbind{name=cpuName, maxIssues, resources, ...}) = 
       let val _ =
           print ("[Generating reservation table for "^arch^"."^cpuName^"]\n")

           (* Compute the number of bits necessary for a field that can count
            * from 0 to n, and compute the extra counts to overflow the field
            *)
           fun getfield(n) =
           let fun bits(0w0, n) = n
                 | bits(w, n)   = bits(Word.>>(w,0w1), n+0w1)
               val bits = bits(Word.fromInt n, 0w0)
               val n'   = Word.toIntX(Word.<<(0w1, bits))
           in  {width=Word.toIntX bits, padding=n'-n-1}
           end

           val checkResource = checkResource("cpu "^cpuName)

           fun genResources(resources) =
           let fun addMask{word,bits,mask=[]} = [{word=word,bits=bits}]
                 | addMask{word,bits,mask={word=w,bits=b}::mask} =
                    if word=w then {word=word,bits=bits + b}::mask
                    else {word=word,bits=bits}::{word=w,bits=b}::mask
               fun loop([], word, bit, res, mask, extra) =
                    {cpu=cpuName, maxIssues=maxIssues,
                     words=word+1, res=res, mask=rev mask, extra=rev extra}
                 | loop((n,r)::rs, word, bit, res, mask, extra) =
                   let val _ = checkResource r
                       val {width, padding} = getfield n
                       val (word, bit, mask, extra) =
                           if bit + width + 1 > MAX_BIT
                           then (word+1, 0, {word=word+1,bits=0w0}::mask,
                                            {word=word+1,bits=0w0}::extra)
                           else (word, bit, mask, extra)
                       val newBit = bit + width + 1
                       val mask   = addMask{word=word, bits=Word.<<(0w1,
                                                 Word.fromInt(bit+width)),
                                            mask=mask}
                       val extra = addMask{word=word, 
                                           bits=Word.<<(Word.fromInt padding,
                                                Word.fromInt bit),
                                           mask=extra}
               in  loop(rs, word, newBit,
                      {resource=r,count=n,word=word,bit=bit}::res, mask, extra)
               end
           in  loop(resources, 0, 0, [], [], [])
           end

       in  genResources resources
       end

       (* Compile all cpu information *)
       val cpuInfo = map compileCPU cpus

       (* number of words needed to represent the state in one cycle *) 
       val W = foldr (fn ({words, ...}, w) => Int.max(words, w)) 0 cpuInfo

       fun many f =
       let fun g n = if n >= W then [] else f n::g(n+1) 
       in  g 0 end  

       (* type of resource *)
       val resTy = DATATYPEdecl([], 
                 [TYPEbind("resource",[],TUPLEty(many (fn _ => WORDty)))])
       val cpuTy = 
             DATATYPEdecl(
              [DATATYPEbind
                  {id="cpu",tyvars=[],mc=NONE,asm=false, field=NONE, 
                   cbs=map (fn CPUbind{name,...} => CONS(name,NONE)) cpus
                  }
              ],
              [])

       val getCPU =
           FUNdecl[FUNbind("cpu", 
             map (fn CPUbind{name,aliases,...} => 
                 CLAUSE([ORpat(map STRINGpat (name::aliases))],
                        NONE, ID name)) cpus
           @ [CLAUSE([IDpat "cpuName"], 
                NONE,
                APP("error",
                  APP("^",
                    TUPLEexp[STRINGexp "unknown architecture ",
                             ID "cpuName"])))] 
                  )]

       val i2s = Int.toString 
       fun w2s w = "0wx"^Word.toString w

       fun get(w, []) = 0w0
         | get(w, {word, bits}::l) = 
             if w = word then Word32.fromInt(Word.toIntX bits) 
             else get(w,l)
 
       (*
        * Function to merge two reservation table entries  
        *)
       val mergeRT = 
       let val arg1 = TUPLEpat(many(fn n => IDpat("x"^i2s n)))
           val arg2 = TUPLEpat(many(fn n => IDpat("y"^i2s n)))
           val arg  = TUPLEpat[arg1,arg2]
           val body = TUPLEexp(many(fn n =>
                          APPexp(IDexp(IDENT(["Word"],"+")), 
                           TUPLEexp[ID("x"^i2s n), ID("y"^i2s n)])))
       in  FUN("mergeRT", arg, body) 
       end

       val zeroRT = VAL("zeroRT",TUPLEexp(many(fn n => WORD32exp 0w0)))

       (* Generate code for each cpu *)
       fun genCode{cpu, words, res, mask, extra, maxIssues} = 
       let fun genRes{resource, count, word, bit} =   
               (resource, 
                  TUPLEexp(many(fn w => if w = word then 
                               WORD32exp(Word32.<<(0w1,Word.fromInt bit)) else 
                               WORD32exp 0w0)))

           val mergeableRT = 
           let val arg1 = TUPLEpat(many (fn n => IDpat("x"^i2s n)))
               val arg2 = TUPLEpat(many (fn n => IDpat("y"^i2s n)))
               val arg  = TUPLEpat[arg1,arg2]
               val body = many (fn n =>
                    let val maskBits = get(n, mask)
                    in  APP("=", 
                          TUPLEexp[
                          APP("Word.andb", 
                            TUPLEexp[PLUS(ID("x"^i2s n),ID("y"^i2s n)),
                                     WORD32exp maskBits]),
                          WORD32exp 0w0])
                    end)
           in  FUN("mergeableRT"^cpu, arg, foldr ANDALSO TRUE body)
           end
       
           val emptyRT = VAL("emptyRT"^cpu,
                           TUPLEexp(many(fn n => WORD32exp(get(n,extra)))))

           val maxIssues = VAL("maxIssues"^cpu,INTexp maxIssues)
           val myResources = map genRes res
           fun getRes res =
           let fun loop([]) = (res,ID "zeroRT")
                 | loop((r,e)::rs) = if r = res then (r,e) else loop rs
           in  loop myResources
           end
           val resources = VAL("resources"^cpu,
                                 RECORDexp(map getRes resources))

       in  SEQdecl[maxIssues,emptyRT,mergeableRT,resources]
       end

       val resourcesPat = RECORDpat(map (fn id => (id,IDpat id)) resources,false)

       (* Generate a pipeline function *)
       fun genPipelineFun(PIPELINEbind(name, clauses)) = 
       let val check = checkResource("pipeline "^name)
           fun trans [] = []
             | trans(IDcycle res::ps) = 
                (check res; ID res::trans ps)
             | trans(ORcycle(a,b)::ps) =
                merge(trans[a], trans[b])@trans ps
             | trans(REPEATcycle(a,n)::ps) = repeat(trans[a],n)@trans ps

           and merge([a],[b]) = [APP("mergeRT",TUPLEexp[a,b])]
             | merge _ = (error("bad expression in pipeline "^name); [])

           and repeat(x,0) = []
             | repeat(x,n) = x@repeat(x,n-1)

           fun genClause(pat,PIPELINE pe) = 
               CLAUSE([resourcesPat,pat],NONE,LISTexp(trans pe,NONE))

       in  FUNdecl[FUNbind("pipeline"^name,map genClause clauses)]
       end

       (* Generate a latency function *)
       fun genLatencyFun(LATENCYbind(name, clauses)) = 
       let fun genClause(pat,e) = CLAUSE([resourcesPat,pat],NONE,e)
       in  FUNdecl[FUNbind("latency"^name,map genClause clauses)]
       end

   in  SEQdecl(resTy::cpuTy::getCPU::zeroRT::mergeRT::
               map genCode cpuInfo @ 
               map genPipelineFun pipelines @
               map genLatencyFun latencies
              )
   end
 
   fun gen compiled_rtls =
   let (* The machine description *)
       val md = RTLComp.md compiled_rtls

       (* name of the structure/signature *)
       val strName = Comp.strname md "SchedProps"  
       val sigName = "SCHEDULING_PROPERTIES"

       val cpus      = Comp.cpus md
       val pipelines = Comp.pipelines md
       val latencies = Comp.latencies md
       val rtDecl = genRT md

       (* query function *)
       val queryFun = RTLComp.queryFun compiled_rtls

       (* default way of handling composite instructions *)
       fun composite{instr,id,ty} = APP("query",ID id)

       (* Arguments to the instruction functor *)
       val args =
           ["structure Instr : "^Comp.signame md "INSTR",
            "structure RegionProps : REGION_PROPERTIES", 
            "structure Asm   : INSTRUCTION_EMITTER where I = Instr",
            "  sharing RegionProps.Region = Instr.Region"
           ]

       (* Definition of the reservation table type *)
       val resTableDefinition = 
           $["structure A = DynArray",
             "type pipeline = resource list",
             "type reservation_table = (int * resource) A.array"
            ]

       (* Make the newTable (per cpu) *)
       val newTable = 
           $["fun newTable emptyRT n = ",
             "  A.array(n,(0,emptyRT)) : reservation_table"
            ]

       (* Make the defUse query function (shared by all cpu types) *)
       val defUse = 
       let val defaultLat = INTexp 0
           fun queryDefUse{instr,rtl,const} =
           let val CONSbind{latency,...} = instr
               val lat = case latency of SOME l => l | NONE => defaultLat
               fun pair(e,l) = TUPLEexp[e,l]

               val (defs,uses) = L.defUse rtl
               val def = 
                    RTL.queryExp rtlmd 
                    {name    = "defUse",
                     reg     = fn (r,_,l) => cons(pair(r,lat),l),
                     fixreg  = fn (r,_,l) => cons(pair(r,lat),l),
                     regs    = fn (rs,_,l) => append(APP("mkSet",rs),l),
                     opnd    = fn (_,l) => l, 
                     lab     = fn (_,l) => l, 
                     imm     = fn (_,l) => l, 
                     cellset = fn (c,l) => append(APP("getCellSetDef",c),l),
                     region  = fn (r,l) => append(APP("getRegionDef",r),l)
                    } (defs, LISTexp([],NONE))
               val use =
                    RTL.queryExp rtlmd 
                    {name    = "defUse",
                     reg     = fn (r,_,l) => cons(r,l),
                     fixreg  = fn (r,_,l) => cons(r,l),
                     regs    = fn (rs,_,l) => append(rs,l),
                     opnd    = fn (x,l) => APP("getOpnd",TUPLEexp[x,l]),
                     lab     = fn (_,l) => l, 
                     imm     = fn (_,l) => l, 
                     cellset = fn (c,l) => append(APP("getCellSetUse",c),l),
                     region  = fn (r,l) => append(APP("getRegionUse",r),l)
                    } (uses, LISTexp([],NONE))

           in  {exp=TUPLEexp[def, use], pat=[]}
           end
           val getOpnd = RTL.queryOpnd rtlmd
                         {name= "getOpnd",                   
                          extraArgs=["rest"],
                          reg = fn r => LISTexp([r],SOME(ID "rest")),
                          imm = fn r => raise Match,
                          opnd= fn r => raise Match,
                          default= ID "rest"
                         }
           val predefined =
                  $["fun mkSet set = map (fn r => (r,0)) set",
                    "fun getRegionDef r = ",
                    "let val (d,u) = RegionProps.writeTo r",
                    "in  map (fn r => (r,~1)) d end",
                    "fun getRegionUse r = RegionProps.readFrom r"
                   ]
           val cellSets = Comp.cellSets md
           val cellSetNames = map (fn CELLdecl{id,...} => id) cellSets
           val getCellSetDef =
               FUN("getCellSetDef",TUPLEpat(map IDpat cellSetNames),
                   foldr (fn (x,LISTexp ([],NONE)) => APP("mkSet",ID x)
                           | (x,e) => APP("@",TUPLEexp[APP("mkSet",ID x),e]))
                         (LISTexp([],NONE)) cellSetNames)
           val getCellSetUse =
               FUN("getCellSetUse",TUPLEpat(map IDpat cellSetNames),
                   foldr (fn (x,LISTexp ([],NONE)) => ID x
                           | (x,e) => APP("@",TUPLEexp[ID x,e]))
                         (LISTexp([],NONE)) cellSetNames)

       in  FUN("defUse",TUPLEpat[IDpat "cpu", IDpat "resources"],
           LETexp([
             SEQdecl(map (fn LATENCYbind(id,_) =>
                  VAL(id, APPexp(APP("latency"^id,ID "resources"), 
                                  ID "cpu"))) latencies),
             queryFun{name="defUseQuery",
                    extraArgs=[],
                    args=["instr"],
                    extraExps=[],
                    localDecls=[getOpnd,predefined,getCellSetDef,getCellSetUse],
                    body=queryDefUse, composite=composite}],
              [ID "defUseQuery"]
            ))
       end

       (* Make the pipeline query function (shared by all cpu types) *)
       val pipelineOf = 
       let val defaultPipeline = LISTexp([], NONE)
           fun queryPipeline{instr,rtl,const} =
           let val CONSbind{pipeline,...} = instr
               val p = case pipeline of SOME p => p | NONE => defaultPipeline
           in  {exp=p, pat=[]}
           end
       in  FUN("pipelineOf",TUPLEpat[IDpat "cpu", IDpat "resources"],
            LETexp([
               SEQdecl(map (fn PIPELINEbind(id,_) =>
                       VAL(id, APPexp(APP("pipeline"^id,ID "resources"), 
                                      ID "cpu"))) pipelines),
               queryFun{name="pipelineQuery",
                        extraArgs=[],
                        args=["instr"],
                        extraExps=[],
                        localDecls=[],
                        body=queryPipeline, 
                        composite=composite}],
              [ID "pipelineQuery"]
            ))
       end


       val findSlotBackward = 
           $["fun findSlotBackward(maxIssues, mergeable)",
             "                 (rt : reservation_table, time, pipeline) = ",
             "let fun search(t) = ",
             "    let fun fits([], t) = true",
             "          | fits(r::rs, t) =", 
             "        mergeable(#2(A.sub(rt,~t)),r)",
             "        andalso fits(rs, t+1)", 
             "    in  if #1(A.sub(rt,~t)) < maxIssues",
             "        andalso fits(pipeline,t) then t else search(t-1)",
             "    end",
             "in  search(time) end"
            ]

       val findSlotForward =
           $["fun findSlotForward (maxIssues, mergeable)",
             "                 (rt : reservation_table, time, pipeline) = ",
             "let fun search(t) = ",
             "    let fun fits([], t) = true",
             "          | fits(r::rs, t) =", 
             "        mergeable(#2(A.sub(rt,t)),r)",
             "        andalso fits(rs, t+1)", 
             "    in  if #1(A.sub(rt,t)) < maxIssues",
             "        andalso fits(pipeline,t) then t else search(t+1)",
             "    end",
             "in  search time end"
            ]

       val insertBackward =
           $["fun insertBackward(rt, time, pipeline) =",
             "let fun update([],t) = ()",
             "      | update(r::rs,t) =",
             "        let val (n,r') = A.sub(rt,~t)",
             "        in  A.update(rt,~t,(n,mergeRT(r,r')));",
             "            update(rs, t+1)",
             "        end",
             "    val _ = update(pipeline, time)",
             "    val (n, r) = A.sub(rt, ~time)",
             "in  A.update(rt, ~time, (n+1, r)) end"
            ]

       val insertForward = 
            $["fun insertForward(rt, time, pipeline) =",
             "let fun update([],t) = ()",
             "      | update(r::rs,t) =",
             "        let val (n,r') = A.sub(rt,t)",
             "        in  A.update(rt,t,(n,mergeRT(r,r')));",
             "            update(rs, t+1)",
             "        end",
             "    val _ = update(pipeline, time)",
             "    val (n, r) = A.sub(rt, time)",
             "in  A.update(rt, time, (n+1, r)) end"
            ]
 
       (* Create the machine info for one architecture *)
       fun genMachineInfo cpu =
       let val cpuAndResources = TUPLEexp[ID cpu,ID("resources"^cpu)]
           val newTable = APP("newTable", ID("emptyRT"^cpu))
           val defUse   = APP("defUse", cpuAndResources)
           val insertArgs = TUPLEexp[ID("maxIssues"^cpu),
                                     ID("mergeableRT"^cpu)]
           val maxIssues   = ID("maxIssues"^cpu)
           val pipeline    = ID "pipeline"
       in  LOCALdecl(
             [VAL("findSlotForward", APP("findSlotForward",insertArgs)),
              VAL("findSlotBackward",  APP("findSlotBackward",insertArgs)),
              VAL("pipeline",     APP("pipelineOf", cpuAndResources))
             ],
             [VAL("forwardinfo"^cpu,
                APP("CPU_INFO", 
                  RECORDexp[("maxIssues",maxIssues),
                            ("newTable",newTable),
                            ("pipeline",pipeline),
                            ("findSlot",ID "findSlotForward"),
                            ("insert",ID "insertForward"),
                            ("defUse",defUse)
                            ])),
              VAL("backwardinfo"^cpu,
                APP("CPU_INFO", 
                  RECORDexp[("maxIssues",maxIssues),
                            ("newTable",newTable),
                            ("pipeline",pipeline),
                            ("findSlot",ID "findSlotBackward"),
                            ("insert",ID "insertBackward"),
                            ("defUse",defUse)
                            ]))
             ]
           )
       end

       val allMachineInfos = 
           SEQdecl(map (fn CPUbind{name,...} => genMachineInfo name) cpus)

       (* The info function *)
       val infoFun = 
          FUN("info",RECORDpat([("cpu",IDpat "cpu"),
                                ("backward",IDpat "backward")],false),
             CASEexp(ID "cpu",
                map (fn CPUbind{name,...} =>
                    CLAUSE([IDpat name],
                       NONE,
                       IFexp(ID "backward",
                          ID("backwardinfo"^name),ID("forwardinfo"^name))))
                cpus))
               
       (* The split copies function. 
        * This must work before RA or after RA 
        *)
       val impl = Comp.hasCopyImpl md
       val implInit = if impl then ", impl=ref NONE" else ""
       val splitCopies =
           $ ["structure Shuffle = Shuffle(I)",
              "fun move{src=I.Direct rs,dst=I.Direct rd} =",
              "     [I.COPY{src=[rs], dst=[rd], tmp=NONE"^implInit^"}]",
              "fun fmove{src=I.FDirect rs,dst=I.FDirect rd} =",
              "     [I.FCOPY{src=[rs], dst=[rd], tmp=NONE"^implInit^"}]",
              "val shuffle = Shuffle.shuffle{mvInstr=move, ea=I.Direct}",
              "val shufflefp = Shuffle.shuffle{mvInstr=fmove, ea=I.FDirect}",
              "fun splitCopies(I.ANNOTATION{i,...}) = splitCopies i",
              "  | splitCopies(I.COPY{src,dst,tmp,...}) =",
              "       shuffle{tmp=tmp, src=src, dst=dst}",
              "  | splitCopies(I.FCOPY{src,dst,tmp,...}) =",
              "       shufflefp{tmp=tmp, src=src, dst=dst}",
              "  | splitCopies i = [i]"
             ]

       (* The functor *)
       val strBody = 
           [$ ["structure I = Instr",
               "structure C = I.C",
               "",
               "type latency = int",
               "type time = int",
               "type architecture = string",
               ""
              ],
            Comp.errorHandler md "SchedProps",
            RTLComp.complexErrorHandlerDef compiled_rtl,
            $ ["",
               "val source = I.SOURCE{}",
               "val sink   = I.SINK{}",
               ""
              ],
            Comp.declOf md "Scheduling",
            rtDecl,
            resTableDefinition,
            $[ "datatype cpu_info =",
               "  CPU_INFO of", 
               "  { maxIssues : int,",
               "    pipeline : I.instruction -> pipeline,",
               "    defUse : I.instruction -> (I.C.cell * latency) list * I.C.cell list,",
               "    newTable : int -> reservation_table,",
               "    findSlot : reservation_table * time * pipeline -> time,",
               "    insert   : reservation_table * time * pipeline -> unit",

               "  }"
             ],
            newTable,
            defUse,
            pipelineOf,
            findSlotForward,
            findSlotBackward,
            insertForward,
            insertBackward,
            allMachineInfos,
            infoFun,
            splitCopies
           ]

   in  Comp.codegen md "scheduling/SchedProps"
         [Comp.mkFct md "SchedProps" args sigName (map Comp.simpDecl strBody)
         ]
   end
end
