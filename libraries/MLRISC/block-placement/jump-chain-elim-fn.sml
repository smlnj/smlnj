(* jump-chain-elim-fn.sml
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies
 *
 * Collapse jumps to jumps.
 *
 * TODO:
 *	check for jumps to the next block.
 *	jump tables (SWITCH edges).
 *)

functor JumpChainElimFn (

    structure CFG : CONTROL_FLOW_GRAPH
    structure InsnProps : INSN_PROPERTIES
      where I = CFG.I

  (* Control flag that when set true allows jumps to labels outside
   * of the CFG to be chained.  Set this false when there are many
   * short jumps to a long jump that exits the CFG.
   *)
    val chainEscapes : bool ref

  (* Control flag that when set true allows the direction (forward or
   * backward) of conditional jumps to be changed.  Set this false
   * when the direction of conditional branches is used to predict
   * the branch.
   *)
    val reverseDirection : bool ref

  ) : sig

    structure CFG : CONTROL_FLOW_GRAPH

    val run : (CFG.cfg * CFG.node list) -> (CFG.cfg * CFG.node list)

  end = struct

    structure CFG = CFG
    structure IP = InsnProps
    structure G = Graph

  (* flags *)
    val disable = MLRiscControl.mkFlag (
	  "disable-jump-chain-elim",
	  "whether jump chain elimination is disabled")
    val dumpCFG = MLRiscControl.mkFlag (
	  "dump-cfg-after-jump-chain-elim",
	  "whether flow graph is shown after jump chain elimination")
    val dumpStrm = MLRiscControl.debug_stream

    fun error msg = MLRiscErrorMsg.error("JumpChainElimFn", msg)

    fun run (cfg, blocks) = let
	  val G.GRAPH{
		  node_info, out_edges, set_out_edges, in_edges,
		  forall_nodes, remove_node, ...
		} = cfg
	  val chainEscapes = !chainEscapes
	  val reverseDirection = !reverseDirection
	(* this flag is set to note that we need to filter out unreachable
	 * blocks after jump chaining.
	 *)
	  val needFilter = ref false
	(* the exit block *)
          val exit = CFG.exitId cfg
	(* map a block ID to a label *)
	  fun labelOf blkId = (case node_info blkId
		 of CFG.BLOCK{labels=ref(lab::_), ...} => lab
		  | CFG.BLOCK{labels, ...} => let
		      val lab = Label.anon()
		      in
			labels := [lab];
			lab
		      end
		(* end case *))
	  fun jumpLabelOf instr = (
                case IP.branchTargets instr
		 of [IP.LABELLED lab] => lab
                  | _ => error ("jumpLabelOf")
	        (* end case *))
	(* given a destination block ID, check to see if it is a block that consists
	 * a single jump instruction.  If so, return the block ID and label of the
	 * block at the end of the jump chain; otherwise return NONE.
	 *)
	  fun followChain blkId = (case node_info blkId
		 of CFG.BLOCK{insns as ref[i], kind=CFG.NORMAL, ...} => (
		    (* a normal block with one instruction *)
		      case out_edges blkId
		       of [e as (_, dst, CFG.EDGE{k=CFG.JUMP, w, a})] =>
			    if ((dst <> exit) orelse chainEscapes)
			      then (
			      (* the instruction must be a jump so transitively follow it
			       * to get the target; but be careful to avoid infinite loops.
			       *)
				set_out_edges (blkId, []);
				case followChain dst
				 of NONE => (
				      set_out_edges (blkId, [e]);
				      SOME(dst, jumpLabelOf i))
				  | (someLab as SOME(dst', lab)) => (
				      insns := [IP.jump lab];
				      set_out_edges (blkId,
					[(blkId, dst', CFG.EDGE{k=CFG.JUMP, w=w, a=a})]);
				      someLab)
				(* end case *))
			      else NONE
			| _ => NONE
		      (* end case *))
		  | _ => NONE
		(* end case *))
	(* For each normal block, check the outgoing edges to see if they
	 * can be redirected.
	 *)
	  fun doBlock (blkId, CFG.BLOCK{insns, kind=CFG.NORMAL, ...}) = let
	        fun setTargets labs = let
		      val (jmp, r) =
			  case !insns of
			      jmp :: r => (jmp, r)
			    | [] => error "setTargets: empty insns"
                      val newJmp = 
			  (case labs
			    of [lab] => IP.setJumpTarget(jmp, lab)
			     | [lab1,lab2] => IP.setBranchTargets{i=jmp, f=lab1, t=lab2}
			     | _ => error "setTargets"
                          (*esac*))
                      in
		        needFilter := true;
			insns := newJmp :: r
                      end
		in
		  case out_edges blkId
		   of [(_, dst, info as CFG.EDGE{k=CFG.JUMP, ...})] => (
			case followChain dst
			 of SOME(dst', lab) => (
			      setTargets [lab];
			      set_out_edges (blkId, [(blkId, dst', info)]))
			  | NONE => ()
			(* end case *))
		    | [(_, dst1, info as CFG.EDGE{k=CFG.BRANCH true, ...}), e2] => (
			case followChain dst1
			 of SOME(dst', lab) => (
			      setTargets [labelOf(#2 e2), lab];
			      set_out_edges (blkId, [(blkId, dst', info), e2]))
			  | NONE => ()
			(* end case *))
		    | [e1, (_, dst2, info as CFG.EDGE{k=CFG.BRANCH true, ...})] => (
			case followChain dst2
			 of SOME(dst', lab) => (
			      setTargets [labelOf(#2 e1), lab];
			      set_out_edges (blkId, [e1, (blkId, dst', info)]))
			  | NONE => ()
			(* end case *))
		    | _ => ()
		  (* end case *)
		end
	    | doBlock _ = ()
          val entry = CFG.entryId cfg
	  fun keepBlock (blkId, _) =
		if null(in_edges blkId) andalso (blkId <> entry)
		  then (remove_node blkId; false)
		  else true
	  val blocks = if !disable
		then blocks
		else (
		  forall_nodes doBlock;
		  if !needFilter then List.filter keepBlock blocks else blocks)
	  in
	    if !dumpCFG
	      then let
		val prBlock = CFG.dumpBlock (!dumpStrm, cfg)
		in
		  TextIO.output(!dumpStrm, "[ after jump-chain elimination ]\n");
		  List.app prBlock blocks
		end
	      else ();
	    (cfg, blocks)
	  end

  end
