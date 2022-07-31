(* label-sig.sml
 *
 * COPYRIGHT (c) 2001 Bell Labs, Lucent Technologies
 *
 * An abstract interface to MLRISC labels.  Labels com in three flavors:
 * global labels have fixed names and are imported/exported from the
 * current compilation unit; local labels have names generated from some
 * given prefix; and anonymous labels have internally generated names that
 * are not in the compilation unit's symbol table.
 *)

signature LABEL =
  sig

    type label

  (* make a global label *)
    val global : string -> label

  (* make a label generator; note that if the prefix string is "", then
   * the standard prefix "L" will be used.
   *)
    val label : string -> unit -> label

  (* make an anonymous label *)
    val anon : unit -> label

  (* label equality, comparisons, and hashing *)
    val same : (label * label) -> bool
    val compare : (label * label) -> order
    val hash : label -> word

  (* label addresses *)
    exception GlobalLabel
    val setAddr : (label * int) -> unit
    val addrOf : label -> int

  (* return a string representation of the label; this function is meant for
   * debugging; use the fmt function for assembly output.
   *)
    val toString : label -> string

  (* format a label for assembly output.  The gPrefix argument is the target
   * ABI's prefix for global symbols (e.g., "_" or "") and the aPrefix is
   * the target assembler's prefix for anonymous labels.  Local labels are
   * emitted using their specified prefxix.
   *)
    val fmt : {gPrefix : string, aPrefix : string}-> label -> string

  (* reset the internal counter used to generate unique IDs for labels; this
   * function should never be called when there are label values still in use.
   *)
    val reset : unit -> unit

  end
