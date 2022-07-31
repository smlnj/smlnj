(* buildFlowgraph.sml
 *
 * COPYRIGHT (c) 2001 Bell Labs, Lucent Technologies
 *)

signature CONTROL_FLOWGRAPH_GEN =
sig

   structure S   : INSTRUCTION_STREAM
   structure I   : INSTRUCTIONS
   structure P   : PSEUDO_OPS
   structure CFG : CONTROL_FLOW_GRAPH
   		where I = I
                  and P = P
   (*
    * This creates an emitter which can be used to build a CFG incrementally
    *)
   type instrStream = 
     (I.instruction, Annotations.annotations, I.C.cellset, CFG.cfg) S.stream

   val build : unit -> instrStream

end


functor BuildFlowgraph 
  (structure Props  : INSN_PROPERTIES
   structure Stream : INSTRUCTION_STREAM
   structure CFG    : CONTROL_FLOW_GRAPH  
			  where I = Props.I
			    and P = Stream.P
  ) : CONTROL_FLOWGRAPH_GEN =
struct
  structure CFG = CFG
  structure P = CFG.P
  structure I = Props.I
  structure G = Graph
  structure S = Stream
  structure Fmt = Format
  structure PB  = PseudoOpsBasisTyp

  exception LabelNotFound

  type instrStream = 
     (I.instruction, Annotations.annotations, CFG.I.C.cellset, CFG.cfg) S.stream

  val dumpCFG = 
      MLRiscControl.mkFlag 
        ("dump-initial-cfg",
         "Dump CFG after instruction selection")
	 
		
  fun error msg = MLRiscErrorMsg.error ("BuildFlowGraph", msg)

  val hashLabel = Word.toInt o Label.hash

  fun build ()  = let
    val cfg as ref(G.GRAPH graph) = ref(CFG.new())
   
    (* list of blocks generated so far *)
    val blockList   = ref ([] : CFG.block list)

    (* list of entry labels to patch successors of ENTRY *)
    val entryLabels = ref ([] : Label.label list)
   
    (* block id associated with a label*)
    val labelMap    = IntHashTable.mkTable(32, LabelNotFound)
    val findLabel   = IntHashTable.find labelMap
    val addLabel    = IntHashTable.insert labelMap

    (* Data in text segment is read-only *)
    datatype segment_t = TEXT | DATA | RO_DATA | BSS | DECLS
    val segmentF    = ref DECLS

    (* the block names *)
    val blockNames   = ref [] : Annotations.annotations ref

    (* can instructions be reordered *)
    val reorder      = ref [] : Annotations.annotations ref

    (* noblock or invalid block has id of ~1 *)
    val noBlock = CFG.newBlock(~1, ref 0.0)

    (* current block being built up *)
    val currentBlock = ref noBlock


    (* add a new block and make it the current block being built up *)
    fun newBlock (freq) = let
      val G.GRAPH graph = !cfg
      val id = #new_id graph ()
      val blk as CFG.BLOCK{annotations, ...} = CFG.newBlock(id, ref freq)
    in
      currentBlock := blk;
      annotations := !blockNames @ !reorder;
      blockList := blk :: !blockList;
      #add_node graph (id, blk);
      blk
    end


    (* get current basic block *)
    fun getBlock () = 
     (case !currentBlock of CFG.BLOCK{id= ~1, ...} => newBlock(1.0) | blk => blk)


    (* ------------------------cluster---------------------------*)
    (* start a new cluster *)
    fun beginCluster _ = 
      (blockList := [];
       entryLabels := [];
       IntHashTable.clear labelMap;
       blockNames := [];
       currentBlock := noBlock)

    (* emit an instruction *)
    fun emit i = let
      val CFG.BLOCK{insns, ...} = getBlock()
      fun terminate() = currentBlock := noBlock;
    in 
      insns := i:: !insns;
      case Props.instrKind(i)
      of Props.IK_JUMP => terminate()
       | Props.IK_CALL_WITH_CUTS => terminate()
       | _ => ()
      (*esac*)
    end

    (* make current block an exit block *)
    fun exitBlock liveout = let
      fun setLiveOut(CFG.BLOCK{annotations, ...}) = 
	annotations := #create CFG.LIVEOUT liveout :: !annotations
    in 
      case !currentBlock
       of CFG.BLOCK{id= ~1, ...} =>
	   (case !blockList
	     of [] => error "exitBlocks"
	      | blk::_ => setLiveOut blk
	   (*esac*))
        | blk => setLiveOut blk
    end (* exitBlock *)


    (* end cluster --- all done *)
    fun endCluster (annotations) = let
      val cfg as G.GRAPH graph = (!cfg before cfg := CFG.new())
      val _ = CFG.init(cfg)		(* create unique ENTRY/EXIT nodes *)

      val ENTRY = hd(#entries graph ())
      val EXIT = hd(#exits graph ())

      fun addEdge(from, to, kind) =
	#add_edge graph (from, to, CFG.EDGE{k=kind, w=ref 0.0, a=ref[]})

      fun addEdgeAn(from, to, kind, an) =
	#add_edge graph (from, to, CFG.EDGE{k=kind, w=ref 0.0, a=ref an})

      fun target lab =
	(case (IntHashTable.find labelMap (hashLabel lab))
	  of SOME bId => bId 
	   | NONE => EXIT)

      val {get=getProb, ...} = MLRiscAnnotations.BRANCH_PROB

      fun jump(from, instr, blocks) = let
	fun branch(targetLab) = let
	  val (_, an) = Props.getAnnotations instr
  	  val an = List.filter
		       (fn (MLRiscAnnotations.BRANCHPROB _) => true | _ => false)
		       an
	  fun next(CFG.BLOCK{id, ...}::_) = id
	    | next [] = error "jump.next"
        in
	    addEdgeAn(from, target targetLab, CFG.BRANCH true, an);
	    addEdge(from, next blocks, CFG.BRANCH false)
        end
      in
	  case Props.branchTargets instr
	   of [Props.ESCAPES] => addEdge(from, EXIT, CFG.EXIT)
	    | [Props.LABELLED lab] => addEdge(from, target lab, CFG.JUMP)
	    | [Props.LABELLED lab, Props.FALLTHROUGH] => branch(lab)
	    | [Props.FALLTHROUGH, Props.LABELLED lab] =>  branch(lab)
	    | targets =>  let
		fun switch(Props.LABELLED lab, n) = 
	            (addEdge(from, target lab, CFG.SWITCH(n)); n+1)
		  | switch _ = error "jump.switch"
              in List.foldl switch 0 targets; ()
              end
      end

      and fallsThru(id, blks) = 
	case blks
	 of [] => addEdge(id, EXIT, CFG.EXIT)
	    | CFG.BLOCK{id=next, ...}::_ => addEdge(id, next, CFG.FALLSTHRU)
	  (*esac*)

	and addEdges [] = ()
	  | addEdges(CFG.BLOCK{id, insns=ref[], ...}::blocks) = fallsThru(id, blocks)
	  | addEdges(CFG.BLOCK{id, insns=ref(instr::_), ...}::blocks) = let
	      fun doJmp () = jump(id, instr, blocks)
	    in
	     case Props.instrKind instr
	      of Props.IK_JUMP => doJmp()
	       | Props.IK_CALL_WITH_CUTS => doJmp()
	       | _ => fallsThru(id, blocks)
	     (*esac*);
	     addEdges(blocks)
	    end
      in
	addEdges (rev(!blockList));
	app (fn lab => addEdge(ENTRY, target lab, CFG.ENTRY)) (!entryLabels);
	let val an = CFG.annotations cfg in  an := annotations @ (!an) end;
	if !dumpCFG
	      then CFG.dump (
		  !MLRiscControl.debug_stream,
		  "after instruction selection", cfg)
	      else ();
	cfg
      end (* endCluster *)


      (* ------------------------annotations-----------------------*)
      (* XXX: Bug: EMPTYBLOCK does not really generate an empty block 
       *	but merely terminates the current block. Contradicts the comment
       *  in instructions/mlriscAnnotations.sig.
       *  It should be (newBlock(1.0); newBlock(1.0); ())
       *)

      (* Add a new annotation *)
      fun addAnnotation a = 
       (case a 
	 of MLRiscAnnotations.BLOCKNAMES names =>
	     (blockNames := names;  newBlock(1.0); ())
	  | MLRiscAnnotations.EMPTYBLOCK => (newBlock(1.0); ())
	  | MLRiscAnnotations.EXECUTIONFREQ f => 
	     (case !currentBlock
	       of CFG.BLOCK{id= ~1, ...} => (newBlock(real f); ())
		| CFG.BLOCK{freq, ...} => freq := real f
	     (*esac*))
	  | a => let 
	       val CFG.BLOCK{annotations,...} = getBlock()
	     in  annotations := a :: !annotations
	     end
       (*esac*))

      (* get annotation associated with flow graph *)
      fun getAnnotations () = CFG.annotations(!cfg)

      (* add a comment annotation to the current block *)
      fun comment msg = 
	case !segmentF 
         of TEXT => addAnnotation (#create MLRiscAnnotations.COMMENT msg)
          | _ => let
		val Graph.GRAPH graph = !cfg
		val CFG.INFO{data, ...} = #graph_info graph
              in data :=  PB.COMMENT msg :: !data
	      end


      (* -------------------------labels---------------------------*)
      (* BUG: Does not respect any ordering between labels and pseudoOps. 
       * This could be a problem with jump tables. 
       *)
      fun addPseudoOp p = let
	val Graph.GRAPH graph = !cfg
	val CFG.INFO{data, decls, ...} = #graph_info graph

	fun addAlignment () = 
	  (case !segmentF
           of DECLS => error "addAlignment: DECLS"
            | TEXT => let
		val CFG.BLOCK{align, ...} = newBlock 1.0
              in align := SOME p
    	      end
	    | _ => data := p :: !data
	  (*esac*))

	fun startSegment(seg) = (data := p :: !data; segmentF := seg)

	fun addData () = data := p :: !data

	fun chkAddData(seg) = let
	    fun errmsg curr = 
		Fmt.format "addPseudoOp: %s in %s segment" [Fmt.STR seg, Fmt.STR curr]
        in
	   case !segmentF
           of DECLS => error(errmsg "DECLS")
	    | TEXT => error(errmsg "TEXT")
	    | _ => data := p :: !data
	  (*esac*)
        end

	fun addDecl() =
	    (case !segmentF
	      of DECLS => decls := p :: !decls
               | _ => data := p :: !data
            (*esac*))
      in
	case p
	of PB.ALIGN_SZ _ => addAlignment()
	 | PB.ALIGN_ENTRY => addAlignment()
	 | PB.ALIGN_LABEL => addAlignment()
	 | PB.DATA_LABEL _ =>
	     (case !segmentF 
	      of TEXT => error "addPseudoOp: DATA_LABEL in TEXT segment"
	       | _ => (data := p:: !data)
	     (*esac*))

	 | PB.DATA_READ_ONLY => startSegment(RO_DATA)
	 | PB.DATA => startSegment(DATA)
	 | PB.TEXT => segmentF := TEXT
	 | PB.BSS => startSegment(BSS)
	 | PB.SECTION _ => 
	    (case !segmentF
	      of TEXT => error "addPseudoOp: SECTION in TEXT segment"
	       | _ => data := p :: !data
	    (*esac*))
	 | PB.REORDER => (reorder := []; newBlock 1.0; ())
	 | PB.NOREORDER => 
	     (reorder := [#create MLRiscAnnotations.NOREORDER ()]; newBlock 1.0; ())

	 | PB.INT _    => chkAddData("INT")
	 | PB.FLOAT _  => chkAddData("FLOAT")
	 | PB.ASCII _  => chkAddData("ASCII")
	 | PB.ASCIIZ _ => chkAddData("ASCIIZ")
	 | PB.SPACE _  => chkAddData("SPACE")
	 | PB.COMMENT _ => addDecl()
	 | PB.IMPORT _ => addDecl()
	 | PB.EXPORT _ => addDecl()
	 | PB.EXT _ => 
	     (case !segmentF 
	       of TEXT => error "EXT in TEXT segment"
		| _ => addDecl()
             (*esac*))
      end

      fun defineLabel lab = 
	(case !segmentF 
	 of TEXT => 
	     (case findLabel (hashLabel lab)
	       of NONE => let
		    fun newBlk () = 
		      (case !currentBlock
			of CFG.BLOCK{id= ~1, ...} => newBlock 1.0
			 | CFG.BLOCK{insns=ref[], ...} => !currentBlock (* probably aligned block *)
			 | _ => newBlock 1.0
		      (*esac*))
		    val CFG.BLOCK{id, labels, ...} = newBlk()
		  in 
		      labels := lab :: !labels;
		      addLabel(hashLabel lab, id)
		  end
		| SOME _ => 
		   error (concat
		     ["multiple definitions of label \"", Label.toString lab, "\""])
	      (*esac*))
 	 | _ => let	
	       (* non-text segment *)
	       val Graph.GRAPH graph = !cfg
	       val CFG.INFO{data, ...} = #graph_info graph
             in
	      data := PB.DATA_LABEL lab :: !data
	     end
       (*esac*))
      
    fun entryLabel lab = (defineLabel lab; entryLabels := lab :: !entryLabels)
  in
    S.STREAM
      { 
         comment       = comment,
         getAnnotations= getAnnotations,
         annotation    = addAnnotation,
         defineLabel   = defineLabel,
         entryLabel    = entryLabel,
         pseudoOp      = addPseudoOp,
         beginCluster  = beginCluster,
         emit          = emit,
         exitBlock     = exitBlock,
         endCluster    = endCluster
      }
  end (* build *)
end
