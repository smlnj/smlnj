(*
 * This updated signature describes the abstractions on ``cells'', which
 * denote storage cells in the machine architecture.
 *
 * Allen Leung (12/2/00)
 *)
signature CELLS_BASIS = 
sig
   type sz           = int (* width in bits *)
   type cell_id      = int (* unique cell identifier *)  
   type register_id  = int (* register id *)
   type register_num = int (* register number *)
    (* Note: register_id and register_num should probably be made into
     * different datatypes with different tags, but FLINT currently boxes 
     * such objects.
     *)

   datatype cellkindInfo = INFO of {name:string, nickname:string}
   datatype cellkindDesc =
        DESC of
        {kind             : cellkind,
         counter          : int ref,
	 dedicated	  : int ref,
	    (* It is sometimes desirable to allocate dedicated 
	     * pseudo registers that will get rewritten to something else,
	     * e.g., the virtual frame pointer. 
	     * Since these registers are never assigned a register  by 
	     * the register allocator, a limited number of these kinds 
	     * of registers may be generated.
	     *)
         low              : int,
         high             : int,
         toString         : register_id -> string,
         toStringWithSize : register_id * sz -> string,
         defaultValues    : (register_id * int) list,
         physicalRegs     : cell Array.array ref,
         zeroReg          : register_id option
        }


   (* Cellkind denotes the types of storage cells.
    * This definition is further augumented by architecture specific 
    * cells descriptions.  Type cellkind is an equality type.
    *)
   and cellkind = 
        GP       (* general purpose register *)
      | FP       (* floating point register *) 
      | CC       (* condition code register *) 

      | MEM      (* memory *)
      | CTRL     (* control dependence *)

      | MISC_KIND of cellkindInfo ref (* client defined *)


   (*
    * A cell is a stateful object reprensenting a storage cell in a 
    * processor.  Cells are partitioned into their kinds, such as
    * GP (general purpose, i.e., integer, registers), * FP 
    * (floating point registers) etc.  Each cell has an unique cell_id
    * that determines its identity.  Its attributes include
    *
    *   1. its color, and
    *   2. other client defined properties, 
    *      which is represented as a property list of annotations.
    *
    *  Note that cell_id and color are two distinct concepts; for example,
    *  two different cells may have the same color.   
    *
    * Type cell is not an equality type.  We provide the function
    * sameCell for testing for object identity, and the function
    * sameColor for testing for color identity.  For most things,
    * sameColor is the right function to use.
    *)
   and cell = 
      CELL of {id   : cell_id,
               col  : cellColor ref, 
               desc : cellkindDesc, 
 	       an   : Annotations.annotations ref
              }
   and cellColor =
         MACHINE of register_id 
       | PSEUDO 
       | ALIASED of cell 
       | SPILLED

   (*
    * Basic functions on cellkinds 
    *)
   val cellkindToString   : cellkind -> string (* name *)
   val cellkindToNickname : cellkind -> string (* abbreviation *)
   val newCellKind        : {name:string,nickname:string} -> cellkind

   (*
    * Basic functions on cells.
    * All functions marked with +++ implicitly chases aliases. 
    *
    * Function register_id returns the current color of a node.
    * The color of a pseudo register is the same as its cell_id.
    * A spilled node is given a color of ~1, so all spilled nodes have
    * the same color.
    *
    * NOTE: distinction between registerId and registerNum:
    * Function register_id returns register_id.  
    * Physical registers in distinct 
    * cell classes are given disjoint register_ids.  So for example,
    * the register id for r0 and f0 in the Alpha are different. 
    *
    * The function, registerNum, on the other hand, returns a
    * register number of a cell that starts from 0 for physical registers.
    * So registerNum r0 = registerNum f0 = 0.   It behaves the same
    * as registerId in other cases.
    *
    * The function physicalRegisterNum is the same as registerNum,
    * except that it is an error to call it on a pseudo or spilled cell.
    * As a rule, use registerId whenever possible.  Function registerNum
    * is used only if you have to deal with machine encoding.
    *)
   val cellId           : cell -> cell_id        (* return cell id *)
   val cellkind         : cell -> cellkind       (* return cellkind *)
   val isConst          : cell -> bool
   val annotations      : cell -> Annotations.annotations ref 
   val sameCell         : cell * cell -> bool    (* object identity *)
   val sameKind         : cell * cell -> bool    (* same cellkind? *)
   val chase            : cell -> cell           (* chase aliases +++ *)
   val sameAliasedCell  : cell * cell -> bool    (* chase aliases +++ *)
   val hashCell         : cell -> word
   val registerId       : cell -> register_id    (* +++ *)     
   val registerNum      : cell -> register_num   (* +++ *)
   val physicalRegisterNum : cell -> int         (* +++ *)
   val sameColor        : cell * cell -> bool    (* color identity +++ *)
   val compareColor     : cell * cell -> order   (* +++ *)
   val toString         : cell -> string         (* pretty print a cell +++ *)
   val toStringWithSize : cell * sz -> string    (* +++ *)

   (* Set the color of the 'from' cell to be the same as
    * the 'to' cell.  The 'from' cell MUST be a pseudo register,
    * and cannot be of kind CONST.
    *)
   val setAlias    : {from: cell, to: cell} -> unit  (* +++ *)

   (*
    * The following abstraction represents a set of cells 
    * indexed by colors.  When two or more cells with the same color
    * exists, we arbitrarily choose a representative.
    * WARNING: while using sorted_cells it is important not to
    * update the colors in the elements, or you'll get wrong results.
    *) 
   structure SortedCells :
     sig
       type sorted_cells 
       val empty      : sorted_cells
       val enter      : cell * sorted_cells -> sorted_cells
       val rmv        : cell * sorted_cells -> sorted_cells
       val member     : cell * sorted_cells -> bool
       val eq         : sorted_cells * sorted_cells -> bool
       val notEq      : sorted_cells * sorted_cells -> bool
       val uniq       : cell list -> sorted_cells
       val difference : sorted_cells * sorted_cells -> sorted_cells
       val intersect  : sorted_cells * sorted_cells -> sorted_cells
       val union      : sorted_cells * sorted_cells -> sorted_cells
       val return     : sorted_cells -> cell list
       val isEmpty    : sorted_cells -> bool
       val emptyIntersection    : sorted_cells * sorted_cells -> bool
       val nonEmptyIntersection : sorted_cells * sorted_cells -> bool
     end 

   (*
    * Hash table indexed by cell id.  
    * IMPORTANT: this table is not indexed by color!
    *)
   structure HashTable : MONO_HASH_TABLE where type Key.hash_key = cell

   (*
    * Hash table indexed by cell color.  
    * IMPORTANT: this table is indexed by color!
    * ALSO: DO NOT change the colors of the cells while using this table!
    *)
   structure ColorTable : MONO_HASH_TABLE where type Key.hash_key = cell

   (* 
    * Cell set represents a map from cellkind to sorted_cells.
    *)
   structure CellSet :
   sig
      type cellset 
      (* cellset functions *)
      val empty  : cellset
      val add    : cell * cellset -> cellset
      val rmv    : cell * cellset -> cellset
      val get    : cellkindDesc -> cellset -> cell list
      val update : cellkindDesc -> cellset * cell list -> cellset
      val map    : {from:cell, to:cell} -> cellset -> cellset

      (* convert cellset into a list of cells *)
      val toCellList : cellset -> cell list

      (* pretty printing *)
      val toString   : cellset -> string
   end

    (*
     * These annotations adds extra definitions and uses to an instruction
     *)
   exception DEF_USE of {cellkind:cellkind, defs:cell list, uses:cell list}
   val DEFUSE : {cellkind:cellkind, defs:cell list, uses:cell list}
                  Annotations.property

    (* Internal use for alias analysis; don't use! *)
   val mem : register_id -> cell

    (* Internal use only! *)
   val show         : cellkindDesc -> register_id -> string
   val showWithSize : cellkindDesc -> register_id * sz -> string

   val array0 : cell Array.array
end

