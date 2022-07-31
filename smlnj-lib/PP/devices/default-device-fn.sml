(* default-device-fn.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * A functor that implements the device properties for a device
 *)

signature DEVICE_OPS =
  sig

    type t
    type style

  (* style operations *)
    val sameStyle    : (style * style) -> bool
    val pushStyle    : (t * style) -> unit
    val popStyle     : t -> unit
    val defaultStyle : t -> style

  (* Output operations *)
    val indent : (t * int) -> unit
    val space : (t * int) -> unit
    val newline : t -> unit
    val string : (t * string) -> unit
    val char : (t * char) -> unit
    val flush : t -> unit

  end

functor DefaultDeviceFn (D : DEVICE_OPS) : sig

    include PP_DEVICE

    structure DevOps : DEVICE_OPS

  (* create a new device with default properties *)
    val new : DevOps.t -> device

  (* create a new device with the specified line width *)
    val newWithWidth : DevOps.t * int -> device

  (* create a new device with the specified properties *)
    val newWithProps : {
	    ops : DevOps.t,
	    maxDepth : int option,
	    ellipses : (string * int),
	    lineW : int option,
	    textW : int option,
	    maxIndent : int option
	  } -> device

  end = struct

    structure DevOps = D

    datatype device = Dev of {
	ops : D.t,
	depthLimit : int option ref,
	ellipses : (string * int) ref,
	lineWid : int option ref,
	textWid : int option ref,
	indentLimit : int option ref
      }

    type style = D.style

    fun newWithProps {ops, maxDepth, ellipses, lineW, textW, maxIndent} = Dev{
	    ops = ops,
	    depthLimit = ref maxDepth,
	    ellipses = ref ellipses,
	    lineWid = ref lineW,
	    textWid = ref textW,
	    indentLimit = ref maxIndent
	  }

    fun newWithWidth (ops, w) = newWithProps {
	    ops = ops,
	    maxDepth = NONE,
	    ellipses = ("...", 3),
	    lineW = SOME w,
	    textW = NONE,
	    maxIndent = NONE
	  }

    fun new ops = newWithProps {
	    ops = ops,
	    maxDepth = NONE,
	    ellipses = ("...", 3),
	    lineW = NONE,
	    textW = NONE,
	    maxIndent = NONE
	  }

  (* style operations *)
    val sameStyle = D.sameStyle
    fun pushStyle (Dev{ops, ...}, sty) = D.pushStyle (ops, sty)
    fun popStyle (Dev{ops, ...}) = D.popStyle ops
    fun defaultStyle (Dev{ops, ...}) = D.defaultStyle ops

  (* Output operations *)
    fun indent (Dev{ops, ...}, n) = D.indent (ops, n)
    fun space (Dev{ops, ...}, n) = D.space (ops, n)
    fun newline (Dev{ops, ...}) = D.newline ops
    fun string (Dev{ops, ...}, s) = D.string (ops, s)
    fun char (Dev{ops, ...}, c) = D.char (ops, c)
    fun flush (Dev{ops, ...}) = D.flush ops

  (* device properties *)
    fun maxDepth (Dev{depthLimit, ...}) = !depthLimit
    fun setMaxDepth (Dev{depthLimit, ...}, d) = depthLimit := d
    fun ellipses (Dev{ellipses, ...}) = !ellipses
    fun setEllipses (Dev{ellipses, ...}, s) = ellipses := (s, String.size s)
    fun setEllipsesWithSz (Dev{ellipses, ...}, s, sz) = ellipses := (s, sz)
    fun lineWidth (Dev{lineWid, ...}) = !lineWid
    fun setLineWidth (Dev{lineWid, ...}, w) = lineWid := w
    fun maxIndent (Dev{indentLimit, ...}) = !indentLimit
    fun setMaxIndent (Dev{indentLimit, ...}, n) = indentLimit := n
    fun textWidth (Dev{textWid, ...}) = !textWid
    fun setTextWidth (Dev{textWid, ...}, n) = textWid := n

  end (* functor DefaultDeviceFn *)
