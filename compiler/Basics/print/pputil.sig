(* pputil.sig
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature PPUTIL =
  sig

  (* CONSISTENT corresponds to HOV and INCONSISTENT corresponds to HV *)
    datatype break_style = CONSISTENT | INCONSISTENT

    val openStyleBox : break_style -> PrettyPrint.stream -> PrettyPrint.indent -> unit

  (* pretty print a separator followed by a cut *)
    val sepWithCut : string -> PrettyPrint.stream -> unit
  (* pretty print a separator followed by a space *)
    val sepWithSpc : string -> PrettyPrint.stream -> unit

    val ppSequence : PrettyPrint.stream -> {
	    sep: PrettyPrint.stream->unit,
	    pr: PrettyPrint.stream->'a->unit,
	    style: break_style
	  } -> 'a list -> unit
    val ppClosedSequence : PrettyPrint.stream -> {
	    front:PrettyPrint.stream->unit,
	    sep:PrettyPrint.stream->unit,
	    back:PrettyPrint.stream->unit,
	    pr:PrettyPrint.stream->'a->unit,
	    style:break_style
	  } -> 'a list -> unit

    val ppBracketedSequence : (string * string * (PrettyPrint.stream -> 'a -> unit))
		    -> PrettyPrint.stream -> 'a list -> unit

    val ppSym : PrettyPrint.stream -> Symbol.symbol -> unit
    val ppString : PrettyPrint.stream -> string -> unit

    val ppvseqNoBox : PrettyPrint.stream
		      -> (PrettyPrint.stream -> 'a -> unit)
		      -> 'a list -> unit

    val ppvseq : PrettyPrint.stream
		 -> int -> string -> (PrettyPrint.stream -> 'a -> unit)
		 -> 'a list -> unit
    val ppvlist : PrettyPrint.stream
		 -> string * string * (PrettyPrint.stream -> 'a -> unit) * 'a list
		 -> unit
    val ppvlist' : PrettyPrint.stream
		 -> string * string * (PrettyPrint.stream -> string -> 'a -> unit)
		      * 'a list
		 -> unit
  (*
    val ppIntPath : PrettyPrint.stream -> int list -> unit
    val ppSymPath : PrettyPrint.stream -> SymPath.path -> unit
    val ppInvPath : PrettyPrint.stream -> InvPath.path -> unit
  *)
    val nl_indent : PrettyPrint.stream -> int -> unit

    val ppTuple: PrettyPrint.stream
		 -> (PrettyPrint.stream -> 'a -> unit) -> 'a list -> unit

    val pps: PrettyPrint.stream -> string -> unit
    val ppi: PrettyPrint.stream -> int -> unit
    val ppcomma : PrettyPrint.stream -> unit
    val ppcomma_nl : PrettyPrint.stream -> unit
    val nl_app : PrettyPrint.stream -> (PrettyPrint.stream -> 'a -> unit)
		 -> 'a list -> unit
    val br_app : PrettyPrint.stream -> (PrettyPrint.stream -> 'a -> unit)
		 -> 'a list -> unit
    val en_pp : PrettyPrint.stream ->
		{break      : {nsp: int, offset: int} -> unit,
		 newline    : unit -> unit,
		 openVBox   : int -> unit,
		 openHVBox  : int -> unit,
		 openHOVBox : int -> unit,
		 closeBox   : unit -> unit,
		 pps        : string -> unit,
		 ppi        : int -> unit}
    val ppArray : PrettyPrint.stream ->
		  (PrettyPrint.stream -> 'a -> unit) * 'a array
		  -> unit

  end (* signature PPUTIL *)
