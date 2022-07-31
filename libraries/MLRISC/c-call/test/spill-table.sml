functor SpillTable
   (val architecture : string  
    val initialSpillOffset : int
    val spillAreaSz : int
   ) : 
sig
  
   val architecture : string
   val beginRA : unit -> unit
   val get     : RAGraph.spillLoc -> int
   val getF    : RAGraph.spillLoc -> int

end =
struct

   structure G = RAGraph

   fun error msg = MLRiscErrorMsg.error(architecture^".SpillTable",msg)
  
   val itow = Word.fromInt

   val architecture = architecture

   exception RegSpills and FregSpills
   val spillOffset = ref initialSpillOffset
   val regspills : int G.SpillLocHashTable.hash_table =
       G.SpillLocHashTable.mkTable(0,RegSpills)
   val fregspills : int G.SpillLocHashTable.hash_table =
       G.SpillLocHashTable.mkTable(0,FregSpills)
   val lookupReg  = G.SpillLocHashTable.lookup regspills
   val enterReg   = G.SpillLocHashTable.insert regspills
   val lookupFreg = G.SpillLocHashTable.lookup fregspills
   val enterFreg  = G.SpillLocHashTable.insert fregspills

   fun beginRA() =
      ((* Reset the regspills/fregspills map by need. *)
       if !spillOffset = initialSpillOffset then ()
       else (G.SpillLocHashTable.clear regspills;
             G.SpillLocHashTable.clear fregspills
            )
       ;
       spillOffset := initialSpillOffset
      )

   fun newOffset offset =
       if offset >= spillAreaSz then error "spill area too small"
       else spillOffset := offset

   (* Get spill location for integer registers *)
   fun get loc =
       lookupReg loc handle _ =>
       let val offset = !spillOffset
       in  newOffset(offset+4);
           enterReg (loc,offset);
           offset
       end

   (* Get spill location for floating point registers *)
   fun getF loc =
       lookupFreg loc handle _ =>
       let val offset = !spillOffset
           val aligned = Word.toIntX (Word.andb(itow (offset+7), itow ~8))
       in
           newOffset(aligned+8);
           enterFreg (loc, aligned);
           aligned
       end

end
