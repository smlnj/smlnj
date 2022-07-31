(* shuffle.sml -- implements the parallel copy instruction as a sequence
 *		of moves. 
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)


functor Shuffle(I : INSTRUCTIONS) :
  sig
    val shuffle : 
      {mvInstr : {dst:I.ea, src:I.ea} -> I.instruction list,
       ea : CellsBasis.cell -> I.ea} 
      ->
	{tmp : I.ea option,
	 dst : CellsBasis.cell list,
	 src : CellsBasis.cell list} 
	-> I.instruction list
  end = 
struct
  structure C = I.C

  datatype obj = TEMP | CELL of CellsBasis.cell

  fun equal (r1, r2) = CellsBasis.sameColor(r1,r2)

  fun equalObj (TEMP, TEMP) = true
    | equalObj (CELL u, CELL v) = equal(u, v)
    | equalObj _ = false

  fun shuffle{mvInstr, ea} {tmp, dst, src} = let
    fun mv{dst, src, instrs} = List.revAppend(mvInstr{dst=dst,src=src}, instrs)

    fun opnd dst = case dst of 
                     TEMP     => Option.valOf tmp 
                   | CELL dst => ea dst

    (* perform unconstrained moves *)
    fun loop((p as (rd,rs))::rest, changed, used, done, instrs) = 
	if List.exists (fn r => equalObj(r, rd)) used then
	   loop(rest, changed, used, p::done, instrs)
	else loop(rest, true, used, done,
                  mv{dst=opnd rd, src=opnd rs, instrs=instrs})
      | loop([], changed, _, done, instrs) = (changed, done, instrs)

    fun cycle([], instrs) = instrs
      | cycle(moves, instrs) =
	(case loop(moves, false, map #2 moves, [], instrs)
	  of (_, [], instrs) => instrs
	   | (true, acc, instrs) => cycle(acc, instrs)
	   | (false, (rd,rs)::acc, instrs) => let
	       fun rename(p as (a,b)) =
                   if equalObj(rd, b) then (a, TEMP) else p
	       val acc' = (rd, rs) :: map rename acc
	       val instrs' = mv{dst=Option.valOf tmp, src=opnd rd, instrs=instrs}
	       val (_, acc'', instrs'') = 
		 loop(acc', false, map #2 acc', [], instrs')
	     in cycle(acc'', instrs'')
	     end
	 (*esac*))

    (* remove moves that have been coalesced. *)
    val rmvCoalesced =
	ListPair.foldl (fn (rd, rs, mvs) =>
			   if equal (rd, rs) then mvs
			   else (CELL rd, CELL rs) :: mvs) []
  in rev (cycle (rmvCoalesced(dst, src), []))
  end
end

