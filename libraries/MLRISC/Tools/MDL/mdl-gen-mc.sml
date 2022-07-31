(*
 * This module generates the machine code emitter of an architecture
 * given a machine description.
 *
 *)
functor MDLGenMC(Comp : MDL_COMPILE) : MDL_GEN_MODULE =
struct
   structure Comp = Comp
   structure Ast  = Comp.Ast
   structure Env  = Comp.Env
   structure T    = Comp.Trans
   structure W    = Word32

   open Ast Comp.Util Comp.Error

   infix << || &&

   val op << = W.<<
   val op || = W.orb
   val op && = W.andb
   val notb  = W.notb

   val itow = W.fromInt
   val itos = Int.toString

   fun gen md =
   let (* name of the functor and signature *)
       val strName = Comp.strname md "MCEmitter"
       val sigName = "INSTRUCTION_EMITTER"

       (* Is debugging turned on? *)
       val debugOn = Comp.debugging md "MC"

       (* Arguments for the functor *)
       val args = ["structure Instr : "^Comp.signame md "INSTR",
		   "structure MLTreeEval : MLTREE_EVAL where T = Instr.T",
		   "structure Stream : INSTRUCTION_STREAM ",
                   "structure CodeString : CODE_STRING"
                  ] @
                  (if debugOn then
                     ["structure Assembler : INSTRUCTION_EMITTER",
                      "  where I = Instr and S = MLTreeStream.S.Stream"
                     ]
                   else [])

       (* Instruction formats that are declared in the description *)
       val formats = Comp.formats md

       (* Instruction widths that are defined in this architecture *)
       val widths = ListMergeSort.uniqueSort Int.compare
                      (foldr (fn ((SOME w,_),l) => w::l | (_,l) => l) []
                             formats)

       (* The Instruction structure *)
       val env = Env.lookupStr (Comp.env md) (IDENT([],"Instruction"))

       (* Make sure that all widths are either 8, 16, 24, or 32 bits *)
       val _ = app
          (fn w =>
              if w < 8 orelse w > 32 orelse w mod 8 <> 0 then
                 error("instruction format must be 8, 16, 24, or 32 bits; found"^itos w)
              else ()) widths

       (* Endianess *)
       val endianess = Comp.endianess md

       (* Name of an emit function *)
       fun emit id = "emit_"^id

       (*
        * For each width N, generate a function eWordN for emitting a word
        * of that width.
        *)
       val emitFuns =
           let val DUMMYbind = FUNbind("dummy",[])
               fun mkEmitWord width =
               let fun f 0 = []
                     | f 8 = [VAL("b8",ID "w")]
                     | f b = VAL("b"^itos b,ID "w")::
                                VAL("w",SLR(ID "w",WORD32exp 0w8))::f(b - 8)
                   fun g 0 = []
                     | g b = APP("eByteW",ID("b"^itos b))::g(b - 8)
                   val debug =
                      if debugOn then
                      [VAL("_", ID "print(\"0x\"^Word32.toString w^\"\\t\")")]
                      else []
                   val body = case endianess of
                                BIG    => g width
                              | LITTLE => rev(g width)
               in  FUNbind("eWord"^itos width,
                       [CLAUSE([IDpat "w"],
                               NONE,
                               LET(debug@rev(f width),SEQexp body))])
               end
           in  FUNdecl(map mkEmitWord widths) end

       (* Functions for emitting the encoding for a cell *)
       val cellFuns =
           let fun mkEmitCell(CELLdecl{id, from, ...}) =
                   FUN'(emit id, IDpat "r",
                      APP("itow", APP("CellsBasis.physicalRegisterNum", ID "r")))
           in  FUNdecl(map mkEmitCell (Comp.cells md)) end

       (*
        * For each datatype T defined in the structure Instruction that
        * has code generation annotations defined, generate a function emit_T.
        *)
       val datatypeFuns =
           let fun WORD w = TYPEDexp(WORD32exp w,WORD32ty)
               fun mkEmitDatatypes([], fbs) = rev fbs
                 | mkEmitDatatypes(DATATYPEbind{id,mc,cbs,...}::dbs, fbs) =
               let fun missing() =
                      error("machine encoding is missing for constructor "^id)
                   fun loop(w, [], cs, found) = (w, rev cs, found)
                     | loop(w, (cb as CONSbind{id, ty, mc, ...})::cbs,
                            cs, found) =
                       let val (e, found) =
                            case (mc, w) of
                              (NONE, SOME(x::_)) => (WORD(itow x), true)
                            | (NONE, SOME []) => (missing(); (WORD 0w0, true))
                            | (NONE, NONE) => (APP("error",STRINGexp id), found)
                            | (SOME(WORDmc w'), SOME(w::l')) =>
                               (if itow w <> w' then
                                  error ("constructor "^id^" encoding is 0x"^
                                         W.toString w'^" but is expecting 0x"^
                                         W.toString(itow w)) else ();
                                (WORD w', true))
                            | (SOME(WORDmc w'), SOME []) => (WORD w', true)
                            | (SOME(WORDmc w'), NONE) => (WORD w', true)
                            | (SOME(EXPmc e), _) => (e, true)
                           val w = case w of NONE => NONE
                                           | SOME(_::w) => SOME w
                                           | SOME [] => (missing(); NONE)
                       in loop(w, cbs,
                               T.mapConsToClause
                                 {prefix=["I"], pat=fn p=>p, exp=e} cb::cs,
                               found)
                       end
                   val (w, cs, found) = loop(mc, cbs, [], false)
                   val _ = case w of
                             SOME(_::_) =>
                              error("Extra machine encodings in datatype "^id)
                           | _ => ()
               in  mkEmitDatatypes(dbs,
                        if found then FUNbind(emit id, cs)::fbs else fbs)
               end
               val dbs = Env.datatypeDefinitions env
           in  FUNdecl(mkEmitDatatypes(dbs,[]))
           end

       (*
        * Generate a formatting function for each machine instruction format
        * defined in the machine description.
        *)
       val formatFuns =
           let fun mkFormat(SOME width, FORMATbind(formatName, fields, NONE)) =
                     mkDefinedFormat(width, formatName, fields)
                 | mkFormat(NONE, FORMATbind(formatName, fields, NONE)) =
                     (error("missing width in format "^formatName);
                      FUNbind(formatName, []))
                 | mkFormat(_, FORMATbind(formatName, fields, SOME e)) =
                     mkFormatFun(formatName, fields, e)

                 (*
                  * Generate an expression that builds up the format
                  *)
               and mkDefinedFormat(totalWidth, formatName, fields) =
               let (* factor out the constant and the variable part *)
                   fun loop([], bit, constant, exps) = (bit, constant, exps)
                     | loop(FIELD{id, width, value, sign, ...}::fs,
                            bit, constant, exps) =
                       let val width =
                               case width of
                                 WIDTH w => w
                               | RANGE(from, to) =>
                                 (if bit <> from then
                                    error("field "^id^
                                          " in format "^formatName^
                                          " starts from bit "^itos from^
                                          " (bit "^itos bit^" expected")
                                  else ();
                                  to - from + 1)
                           val mask = (0w1 << Word.fromInt width) - 0w1
                           val (constant, exps) =
                               case value of
                                  SOME v =>
                                   (if (v && (notb mask)) <> 0w0 then
                                      error("value 0x"^W.toString v^
                                            "in field "^id^
                                            " is out of range")
                                    else ();
                                    (constant || (v << Word.fromInt bit),
                                     exps))
                               | NONE =>
                                 let val e = ID id
                                     val e = if sign = UNSIGNED then e else
                                               ANDB(e,WORD32exp mask)
                                     val e = SLL(e,WORD32exp(itow bit))
                                 in  (constant, e::exps) end
                       in  loop(fs, bit+width, constant, exps) end
                   val (realWidth, constant, exps) =
                           loop(rev fields, 0, 0w0, [])
               in  if realWidth <> totalWidth then
                      error("format "^formatName^" is declared to have "^
                            itos totalWidth^" bits but I counted "^
                            itos realWidth)
                   else ();
                   mkFormatFun(formatName, fields,
                               APP("eWord"^itos totalWidth,
                                   foldr PLUS (WORD32exp constant) exps))
               end

                 (* Generate a format function that includes implicit
                  * argument conversions.
                  *)
               and mkFormatFun(id, fields, exp) =
                   FUNbind(id, [CLAUSE(
                     [RECORDpat(foldr (fn (FIELD{id="",...}, fs) => fs
                                       | (FIELD{value=SOME _,...}, fs) => fs
                                       | (FIELD{id,...},fs) => (id,IDpat id)::fs                                     ) [] fields, false)],
                     NONE,
                     LET(foldr (fn (FIELD{id,cnv=NOcnv, ...},ds) => ds
                                 | (FIELD{id,cnv=CELLcnv k, ...},ds) =>
                                     VAL(id, APP(emit k,ID id))::ds
                                 | (FIELD{id,cnv=FUNcnv f, ...},ds) =>
                                     VAL(id, APP(emit f,ID id))::ds
                               ) [] fields, exp))])
           in FUNdecl(map mkFormat (Comp.formats md)) end

       (* The main emitter function *)
       val emitInstrFun =
           let fun mkEmitInstr(cb as CONSbind{id, mc, ...}) =
                   T.mapConsToClause
                      {prefix=["I"],pat=fn p=>p,
                       exp=case mc of
                             SOME(EXPmc e) => e
                           | _ => APP("error", STRINGexp id)
                      } cb
               val instructions = Comp.instructions md
           in  FUNdecl[FUNbind("emitInstr", map mkEmitInstr instructions)]
           end


       (* Body of the module *)
       val strBody =
       [$["structure I = Instr",
          "structure C = I.C",
          "structure Constant = I.Constant",
          "structure T = I.T",
          "structure S = Stream",
          "structure P = S.P",
          "structure W = Word32",
          "",
          "(* "^Comp.name md^" is "^
              (case endianess of BIG => "big" | LITTLE => "little")^
              " endian *)",
          ""
         ],
        Comp.errorHandler md "MC",
        $["fun makeStream _ =",
          "let infix && || << >> ~>>",
          "    val op << = W.<<",
          "    val op >> = W.>>",
          "    val op ~>> = W.~>>",
          "    val op || = W.orb",
          "    val op && = W.andb",
          "    val itow = W.fromInt",
          "    fun emit_bool false = 0w0 : W.word",
          "      | emit_bool true = 0w1 : W.word",
          "    val emit_int = itow",
          "    fun emit_word w = w",
          "    fun emit_label l = itow(Label.addrOf l)",
          "    fun emit_labexp le = itow(MLTreeEval.valueOf le)",
          "    fun emit_const c = itow(Constant.valueOf c)",
          "    val w32ToByte = Word8.fromLarge o Word32.toLarge",
          "    val loc = ref 0",
          "",
          "    (* emit a byte *)",
          "    fun eByte b =",
          "      let val i = !loc in loc := i + 1; CodeString.update(i,b) end",
          "",
          "    (* emit the low order byte of a word *)",
          "    (* note: fromLargeWord strips the high order bits! *)",
          "    fun eByteW w =",
          "      let val i = !loc",
          "      in loc := i + 1; CodeString.update(i, w32ToByte w) end",
          "",
          "    fun doNothing _ = ()",
	  "    fun fail _ = raise Fail \"MCEmitter\"",
          "    fun getAnnotations () = error \"getAnnotations\"",
          "",
          "    fun pseudoOp pOp = P.emitValue{pOp=pOp, loc= !loc,emit=eByte}",
          "",
          "    fun init n = (CodeString.init n; loc := 0)",
          "",
             (if debugOn then
               "val S.STREAM{emit=asm,...} = Assembler.makeStream()"
              else ""
             )
       ],
         emitFuns,
         cellFuns,
         datatypeFuns,
         formatFuns,
         Comp.declOf md "MC",
       $["    fun emitter instr =",
         "    let"
        ],
         emitInstrFun,
       $["    in",
          (if debugOn then
          "        emitInstr instr; asm instr"
           else
          "        emitInstr instr"
          ),
          "    end",
          "",
	  "fun emitInstruction(I.ANNOTATION{i, ...}) = emitInstruction(i)",
	  "  | emitInstruction(I.INSTR(i)) = emitter(i)",
	  "  | emitInstruction(I.LIVE _)  = ()",
	  "  | emitInstruction(I.KILL _)  = ()",
	  "| emitInstruction _ = error \"emitInstruction\"",
          "",
          "in  S.STREAM{beginCluster=init,",
          "             pseudoOp=pseudoOp,",
          "             emit=emitInstruction,",
          "             endCluster=fail,",
          "             defineLabel=doNothing,",
          "             entryLabel=doNothing,",
          "             comment=doNothing,",
          "             exitBlock=doNothing,",
          "             annotation=doNothing,",
          "             getAnnotations=getAnnotations",
          "            }",
          "end"
        ]
      ]

   in  Comp.codegen md "emit/MC"
         [Comp.mkFct md "MCEmitter" args sigName strBody]
   end
end
