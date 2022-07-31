(*
 * Description of cell and other updatable cells.
 *
 * -- Allen.
 *)

(*
 * This functor is applied to create the cells structure for an  architecture
 *)
functor Cells
   (exception Cells
    val firstPseudo   : int
    val cellKindDescs : (CellsBasis.cellkind * CellsBasis.cellkindDesc) list
    val cellSize      : int
   ) : CELLS =
struct

   open CellsBasis

   exception Cells = Cells

   val i2s = Int.toString

   fun error msg = MLRiscErrorMsg.error(exnName Cells, msg)

   val cellkinds	 = map (fn (kind,_) => kind) cellKindDescs
   val firstPseudo	 = firstPseudo
   val maxDedicatedCells = 256
   val firstName         = firstPseudo + maxDedicatedCells
   val name		 = ref firstName
(*   val cellCounter = name *)

   val _ = app (fn (_, desc as DESC{physicalRegs, high, low, ...}) =>
                let val n = high - low + 1
                in  if n <= 0 then ()
                    else let val a = Array.tabulate(n, fn nth =>
                                   let val reg = nth + low
                                   in  CELL{id=reg, col=ref(MACHINE reg),
                                       an=ref [], desc=desc}
                                   end)
                         in  physicalRegs := a
                         end
                end) cellKindDescs

   fun nextName() = let val id = !name in name := !name + 1; id end

   fun desc(k:cellkind) =
   let fun loop [] = error("missing info for "^cellkindToString k)
         | loop((kind,info)::defs) =
           if kind = k then info else loop defs
   in  loop cellKindDescs end

   val cellkindDesc = desc

   fun cellRange k =
   let val DESC{low,high,...} = desc k
   in  {low=low,high=high} end

   fun Reg k = let
         val desc as DESC{low,kind,physicalRegs,...} = desc k
         in
	   fn nth => (Array.sub(!physicalRegs,nth) handle _ => raise Cells)
         end

   fun Regs k =
   let val Reg = Reg k
       fun loop{from, to, step} =
           if from > to then []
           else Reg from :: loop{from=from+step, to=to, step=step}
   in  loop end

   fun Cell k =
   let val desc as DESC{low,kind,physicalRegs,...} = desc k
   in  fn reg =>
           Array.sub(!physicalRegs,reg - low)  handle _ => raise Cells
   end

   val GPReg = Reg GP
   val FPReg = Reg FP

   (* Counters *)
   fun newCell k =
       let val desc as DESC{counter,...} = desc k
       in  fn _ =>
           let val r = !name
           in  name := r + 1;
               counter := !counter + 1;
               CELL{id=r, col=ref PSEUDO, an=ref [], desc=desc}
           end
       end

   local val desc as DESC{counter, ...} = desc GP
   in fun newReg _ =
      let val r = !name
      in  name := r + 1;
          counter := !counter + 1;
          CELL{id=r, col=ref PSEUDO, an=ref [], desc=desc}
      end
   end

   local val desc as DESC{counter, ...} = desc FP
   in fun newFreg _ =
      let val r = !name
      in  name := r + 1;
          counter := !counter + 1;
          CELL{id=r, col=ref PSEUDO, an=ref [], desc=desc}
      end
   end

   fun newDedicatedCell k =
       let val desc as DESC{dedicated,...} = desc k
       in  fn _ =>
           let val d = !dedicated
           in  dedicated := d + 1;
	       if d >= maxDedicatedCells then
		 error "too many dedicated cells"
	       else
               CELL{id=firstPseudo+d, col=ref PSEUDO, an=ref [], desc=desc}
           end
       end

   fun newVar (CELL{desc, an, ...}) =
   let val r = !name
   in  name := r + 1;
       CELL{id=r, col=ref PSEUDO, an=ref(!an), desc=desc}
   end

   fun cloneCell c =
   let val CELL{desc, an, col, ...} = chase c
       val r = !name
   in  name := r + 1;
       CELL{id=r, col=ref(!col), an=ref(!an), desc=desc}
   end

   fun numCell k = let val DESC{counter, ...} = desc k
                   in fn () => !counter end

   fun maxCell() = !name

   fun reset() =
       (app (fn (_,DESC{counter, ...}) => counter := 0) cellKindDescs;
        name := firstName
       )

   type cellset = CellSet.cellset
   val empty   = CellSet.empty
   fun getCellsByKind (k : cellkind) = CellSet.get (desc k)
   fun updateCellsByKind (k : cellkind) = CellSet.update (desc k)
   val getReg  = getCellsByKind GP
   val getFreg = getCellsByKind FP
   val addReg  = CellSet.add
   val addFreg = CellSet.add
   val rmvReg  = CellSet.rmv
   val rmvFreg = CellSet.rmv

  (* Misc *)
   fun zeroReg k =
   let val desc as DESC{zeroReg, physicalRegs, low, ...} = desc k
   in  case zeroReg of
         NONE => NONE
       | SOME r => SOME(Array.sub(!physicalRegs, r))
   end

   fun defaultValues k =
   let val DESC{defaultValues, ...} = desc k
   in  defaultValues end

  (* dummy values for now; these get redefined for each architecture *)
   val stackptrR = GPReg 0
   val asmTmpR = GPReg 0
   val fasmTmp = FPReg 0
   val cellSize = cellSize
end
