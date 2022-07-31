(*
 * This updated signature describes the abstractions on ``cells'', which
 * denote storage cells in the machine architecture.
 *
 * Allen Leung (12/2/00)
 *)


(*
 * Things that are architecture specific.
 *)
signature CELLS = sig

   val cellkinds : CellsBasis.cellkind list  
   (* list of all the cellkinds *)

   val firstPseudo : CellsBasis.cell_id      
   (* first pseudo register *)
  
   val cellkindDesc : CellsBasis.cellkind -> CellsBasis.cellkindDesc 
   (* find descriptor *)				  

   val cellRange : CellsBasis.cellkind -> {low:int, high:int}
   (* given a cellkind returns its encoding range *)

   val Reg   : CellsBasis.cellkind -> (CellsBasis.register_num -> CellsBasis.cell)
   (* Returns the nth physical register of the given kind,
    * raises Cells if there are no physical register of the given number.
    * Also raises Cells if the given number if outside of the range.
    * NOTE: this function returns the same cell for the 
    * same argument every time.   See also the function cloneCell below
    *)

   val Regs  : 
     CellsBasis.cellkind -> 
        {from : CellsBasis.register_num, 
	 to   : CellsBasis.register_num, 
	 step : int
	 } ->
	    CellsBasis.cell list
  (* return a list of cells *)

   val Cell  : CellsBasis.cellkind -> (CellsBasis.register_id -> CellsBasis.cell) 
   (* Same as Reg but we take the id instead.
    * So, registerNum(Reg k r) = r, and
    *     registerId(Cell k id) = id
    *)

   val GPReg : int -> CellsBasis.cell (* abbreviation for Reg GP *)
   val FPReg : int -> CellsBasis.cell (* abbreviation for Reg FP *)

       (*
        * Generate a new cell for a virtual register.  The new cell
        * is a pseudo register that is distinct from any other registers.
        * IMPORTANT: if you are using newCell, it is important to 
        * partially apply it first to get a function.  Then uses this
        * function generate new cells.  The first application takes
        * time.
        *)
   val newCell   : CellsBasis.cellkind -> ('a -> CellsBasis.cell)
   val newReg    : 'a -> CellsBasis.cell  (* abbreviation for newCell GP *)
   val newFreg   : 'a -> CellsBasis.cell  (* abbreviation for newCell FP *)

   val newDedicatedCell : CellsBasis.cellkind -> ('a -> CellsBasis.cell)

       (* lookup the number of virtual registers in a CellsBasis.cellkind *)
   val numCell   : CellsBasis.cellkind -> (unit -> int) 

       (* the next virtual register name *) 
   val maxCell   : unit -> CellsBasis.cell_id
     
       (* Given a cell c, create a new pseudo register that has the same 
        * cellkind as c, and a new property list initialized 
        * with the contents of c's properity list.
        * Note: the numCell kind is NOT updated!
        *)
   val newVar : CellsBasis.cell -> CellsBasis.cell

       (* This is the same as above, except that if the original
        * cell is colored, then the new cell has the same color.
        * Note that it is possible to have two cells (or more) with
        * the same physical color.  In these cases they can be used
        * to denote the same register, but they have different identities,   
        * and different property lists.  This may be useful for 
        * representing the same register used in different situations.  
        * See the function Reg above.
        *)
   val cloneCell : CellsBasis.cell -> CellsBasis.cell

       (* Reset all counters. *) 
   val reset     : unit -> unit 

       (* Abbreviations for cellsets *)
   type cellset = CellsBasis.CellSet.cellset 

   val empty          : cellset
   val getReg         : cellset -> CellsBasis.cell list 
   val addReg         : CellsBasis.cell * cellset -> cellset 
   val rmvReg         : CellsBasis.cell * cellset -> cellset
   val getFreg        : cellset -> CellsBasis.cell list 
   val addFreg        : CellsBasis.cell * cellset -> cellset
   val rmvFreg        : CellsBasis.cell * cellset -> cellset

   val getCellsByKind    : CellsBasis.cellkind -> cellset -> CellsBasis.cell list
   val updateCellsByKind : CellsBasis.cellkind
			    -> cellset * CellsBasis.cell list
			      -> cellset

       (* Return a register that is always zero on the architecture,
        * if one exists.  IMPORTANT: each call returns the same cell.
        * See also cloneCell above.
        *)
   val zeroReg    : CellsBasis.cellkind -> CellsBasis.cell option  
                           
   val defaultValues : CellsBasis.cellkind -> (CellsBasis.register_id * int) list

   val stackptrR     : CellsBasis.cell (* stack pointer register *)
   val asmTmpR       : CellsBasis.cell (* assembly temporary *)
   val fasmTmp       : CellsBasis.cell (* floating point temporary *)
   val cellSize      : int
end



