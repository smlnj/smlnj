(* default-device-fn.sml
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * A functor that implements the device properties for a device
 *)

signature DEVICE_OPS =
  sig

    type t
    type style
    type token

  (* style operations *)
    val pushStyle    : (t * style) -> unit
    val popStyle     : t -> unit
    val defaultStyle : t -> style

  (* Output operations *)
    val indent : (t * int) -> unit
    val space : (t * int) -> unit
    val newline : t -> unit
    val string : (t * string) -> unit
    val token : (t * token) -> unit
    val flush : t -> unit

  end

functor DefaultDeviceFn (D : DEVICE_OPS) : sig

    include PP_DEVICE
      where type style = D.style
      where type token = D.token

    structure DevOps : DEVICE_OPS

  (* create a new device with default properties *)
    val new : DevOps.t -> device

  (* create a new device with the specified line width *)
    val newWithWidth : DevOps.t * int -> device

  (* create a new device with the specified properties *)
    val newWithProps : {
	    ops : DevOps.t,
	    lineW : int option,
	    textW : int option,
	    maxIndent : int option
	  } -> device

  end = struct

    structure DevOps = D

    datatype device = Dev of {
	ops : D.t,
	lineWid : int option ref,
	textWid : int option ref,
	indentLimit : int option ref
      }

    type style = D.style
    type token = D.token

    fun newWithProps {ops, lineW, textW, maxIndent} = Dev{
	    ops = ops,
	    lineWid = ref lineW,
	    textWid = ref textW,
	    indentLimit = ref maxIndent
	  }

    fun newWithWidth (ops, w) = newWithProps {
	    ops = ops,
	    lineW = SOME w,
	    textW = NONE,
	    maxIndent = NONE
	  }

    fun new ops = newWithProps {
	    ops = ops,
	    lineW = NONE,
	    textW = NONE,
	    maxIndent = NONE
	  }

  (* style operations *)
    fun pushStyle (Dev{ops, ...}, sty) = D.pushStyle (ops, sty)
    fun popStyle (Dev{ops, ...}) = D.popStyle ops
    fun defaultStyle (Dev{ops, ...}) = D.defaultStyle ops

  (* Output operations *)
    fun indent (Dev{ops, ...}, n) = D.indent (ops, n)
    fun space (Dev{ops, ...}, n) = D.space (ops, n)
    fun newline (Dev{ops, ...}) = D.newline ops
    fun string (Dev{ops, ...}, s) = D.string (ops, s)
    fun token (Dev{ops, ...}, t) = D.token (ops, t)
    fun flush (Dev{ops, ...}) = D.flush ops

  (* device properties *)
    fun lineWidth (Dev{lineWid, ...}) = !lineWid
    fun setLineWidth (Dev{lineWid, ...}, w) = lineWid := w
    fun maxIndent (Dev{indentLimit, ...}) = !indentLimit
    fun setMaxIndent (Dev{indentLimit, ...}, n) = indentLimit := n
    fun textWidth (Dev{textWid, ...}) = !textWid
    fun setTextWidth (Dev{textWid, ...}, n) = textWid := n

  end (* functor DefaultDeviceFn *)
