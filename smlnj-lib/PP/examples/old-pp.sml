(* old-pp.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * An implementation of the SML/NJ's old PP interface on top of the PP library.
 *)

signature OLD_PRETTYPRINT =
  sig
    type ppstream
    type ppconsumer = {
	consumer : string -> unit,
	linewidth : int,
	flush : unit -> unit
      }

    datatype break_style = CONSISTENT | INCONSISTENT

    exception PP_FAIL of string

    val mk_ppstream	: ppconsumer -> ppstream
    val dest_ppstream	: ppstream -> ppconsumer
    val add_break	: ppstream -> int * int -> unit
    val add_newline	: ppstream -> unit
    val add_string	: ppstream -> string -> unit
    val begin_block	: ppstream -> break_style -> int -> unit
    val end_block	: ppstream -> unit
    val clear_ppstream	: ppstream -> unit
    val flush_ppstream	: ppstream -> unit
    val with_pp		: ppconsumer -> (ppstream -> unit) -> unit
    val pp_to_string	: int -> (ppstream -> 'a -> unit) -> 'a -> string

  end;

structure OldPrettyPrint :> OLD_PRETTYPRINT =
  struct

    type ppconsumer = {
	consumer : string -> unit,
	linewidth : int,
	flush : unit -> unit
      }

    structure Dev =
      struct
	type device = ppconsumer
	type style = unit
	fun sameStyle _ = true
	fun pushStyle _ = ()
	fun popStyle _ = ()
	fun defaultStyle _ = ()
	fun maxDepth _ = NONE
        fun setMaxDepth _ = ()
	fun ellipses _ = ("", 0)
	fun setEllipses _ = ()
	fun setEllipsesWithSz _ = ()
	fun lineWidth {consumer, linewidth, flush} = SOME linewidth
	fun setLineWidth _ = ()
	fun maxIndent _ = NONE
	fun setMaxIndent _ = ()
	fun textWidth _ = NONE
	fun setTextWidth _ = ()
	fun space ({consumer, linewidth, flush}, n) =
	      consumer (StringCvt.padLeft #" " n "")
	val indent = space
	fun newline {consumer, linewidth, flush} = consumer "\n"
	fun string ({consumer, linewidth, flush}, s) = consumer s
	fun char ({consumer, linewidth, flush}, c) = consumer(str c)
	fun flush {consumer, linewidth, flush} = flush()
      end

    structure PP = PPStreamFn(structure Token = StringToken structure Device = Dev)

    datatype ppstream = STRM of {
	consumer : ppconsumer,
	strm : PP.stream
      }

    datatype break_style = CONSISTENT | INCONSISTENT

    exception PP_FAIL of string

    fun mk_ppstream ppc	= STRM{
	    consumer = ppc,
	    strm = PP.openStream ppc
	  }
    fun dest_ppstream (STRM{consumer, ...}) = consumer
    fun add_break (STRM{strm, ...}) (nsp, offset) =
	  PP.break strm {nsp=nsp, offset=offset}
    fun add_newline (STRM{strm, ...}) = PP.newline strm
    fun add_string (STRM{strm, ...}) s = PP.string strm s
    fun begin_block (STRM{strm, ...}) CONSISTENT indent =
	  PP.openHVBox strm (PP.Rel indent)
      | begin_block (STRM{strm, ...}) INCONSISTENT indent =
	  PP.openHOVBox strm (PP.Rel indent)
    fun end_block (STRM{strm, ...}) = PP.closeBox strm
    fun clear_ppstream(STRM{strm, ...}) =
	  raise Fail "clear_ppstream not implemented"
    fun flush_ppstream (STRM{strm, ...}) = PP.flushStream strm
    fun with_pp ppc f = let
	  val (ppStrm as (STRM{strm, ...})) = mk_ppstream ppc
	  in
	    f ppStrm;
	    PP.closeStream strm
	  end
    fun pp_to_string wid ppFn obj = let
	  val l = ref ([] : string list)
	  fun attach s = l := s :: !l
	  in
	    with_pp {
		consumer = attach, linewidth = wid, flush = fn()=>()
	      } (fn ppStrm => ppFn ppStrm obj);
	    String.concat(List.rev(!l))
	  end

  end;

