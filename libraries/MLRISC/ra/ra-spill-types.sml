functor RASpillTypes(I : INSTRUCTIONS) =
struct

   structure G = RAGraph
   structure C = I.C
   structure CB = CellsBasis

   type copyInstr =
          (CB.cell list * CB.cell list) * I.instruction -> I.instruction list

   (*
    * Spill the value associated with reg into spillLoc.
    * All definitions of instr should be renamed to a new temporary newReg. 
    *)
   type spill =
      {instr    : I.instruction,       (* instruction where spill is to occur *)
       reg      : CB.cell,              (* register to spill *)
       spillLoc : G.spillLoc,          (* logical spill location *)
       kill     : bool,                (* can we kill the current node? *)
       annotations : Annotations.annotations ref (* annotations *)
      } ->
      {code     : I.instruction list,  (* instruction + spill code *)
       proh     : CB.cell list,         (* prohibited from future spilling *)
       newReg   : CB.cell option        (* the spilled value is available here *)
      }

   (* Spill the register src into spillLoc.
    * The value is originally from register reg.
    *)
   type spillSrc =
      {src      : CB.cell,              (* register to spill from *)
       reg      : CB.cell,              (* the register *)
       spillLoc : G.spillLoc,         (* logical spill location *)
       annotations : Annotations.annotations ref (* annotations *)
      } -> I.instruction list          (* spill code *)

   (*
    * Spill the temporary associated with a copy into spillLoc
    *)
   type spillCopyTmp =
      {copy     : I.instruction,       (* copy to spill *)
       reg      : CB.cell,              (* the register *)
       spillLoc : G.spillLoc,          (* logical spill location *)
       annotations : Annotations.annotations ref (* annotations *)
      } -> I.instruction               (* spill code *)

   (*
    * Reload the value associated with reg from spillLoc.
    * All uses of instr should be renamed to a new temporary newReg.
    *)
   type reload =
      {instr    : I.instruction,       (* instruction where spill is to occur *)
       reg      : CB.cell,              (* register to spill *)
       spillLoc : G.spillLoc,          (* logical spill location *)
       annotations : Annotations.annotations ref (* annotations *)
      } ->
      {code     : I.instruction list,  (* instr + reload code *)
       proh     : CB.cell list,         (* prohibited from future spilling *)
       newReg   : CB.cell option        (* the reloaded value is here *)
      }

   (*
    * Rename all uses fromSrc to toSrc
    *)
   type renameSrc =
      {instr    : I.instruction,       (* instruction where spill is to occur *)
       fromSrc  : CB.cell,              (* register to rename *)
       toSrc    : CB.cell               (* register to rename to *)
      } ->
      {code     : I.instruction list,  (* renamed instr *)
       proh     : CB.cell list,         (* prohibited from future spilling *)
       newReg   : CB.cell option        (* the renamed value is here *)
      }

   (* Reload the register dst from spillLoc. 
    * The value is originally from register reg.
    *)
   type reloadDst =
      {dst      : CB.cell,              (* register to reload to *)
       reg      : CB.cell,              (* the register *)
       spillLoc : G.spillLoc,          (* logical spill location *)
       annotations : Annotations.annotations ref (* annotations *)
      } -> I.instruction list          (* reload code *)

end
