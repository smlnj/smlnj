(*
 * This is a generic instruction stream datatype.
 * Components such as assemblers, machine code emitters, instruction
 * selection modules communicate with each via this interface.
 *
 * -- Allen
 *)

signature INSTRUCTION_STREAM =
sig

   structure P : PSEUDO_OPS
   datatype ('a,'b,'c,'d) stream =
      STREAM of
      { beginCluster: int -> unit,             (* start new compilation unit *)
        endCluster  : 'b -> 'd,                      (* end compilation unit *)
        emit        : 'a -> unit,                        (* emit instruction *)
        pseudoOp    : P.pseudo_op -> unit,               (* emit a pseudo op *)
        defineLabel : Label.label -> unit,           (* define a local label *)
        entryLabel  : Label.label -> unit,       (* define an external label *)
        comment     : string -> unit,                        (* emit comment *)
        annotation  : Annotations.annotation -> unit,      (* add annotation *)
        getAnnotations: unit -> Annotations.propList ref,  (* get annotations*)
        exitBlock   : 'c -> unit              (* mark the end of a procedure *)
      }

   (* Note:
    o  Each compilation unit should be wrapped between beginCluster/endCluster.
     
    o  The method annotation adds an annotation to the current basic block,
       not to the current instruction. 
       
    o  The method comment add a comment to the current basic block.
       Usually comment(msg) is the same as 
          annotation(BasicAnnotations.COMMENT msg).
    *)

end
