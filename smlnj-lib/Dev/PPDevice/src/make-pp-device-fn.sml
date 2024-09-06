(* make-pp-device-fn.sml
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * A functor that implements the device properties for a device
 *)

functor MakePPDeviceFn (D : PP_DEVICE_OPS) : sig

    include PP_DEVICE
      where type style = D.style
      where type token = D.token

    type t = D.t

  (* create a new device with default properties *)
    val new : D.t -> device

  (* create a new device with the specified line width *)
    val newWithWidth : D.t * int -> device

  (* create a new device with the specified properties *)
    val newWithProps : {
	    ops : D.t,
	    lineW : int option,
	    textW : int option,
	    maxIndent : int option
	  } -> device

  (* return the device-ops component of the device *)
    val devOps : device -> D.t

  end = struct

    type t = D.t

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

  (* return the device-ops component of the device *)
    fun devOps (Dev{ops, ...}) = ops

  (* style operations *)
    fun pushStyle (Dev{ops, ...}, sty) = D.pushStyle (ops, sty)
    fun popStyle (Dev{ops, ...}) = D.popStyle ops
    fun withStyle (Dev{ops, ...}, sty, f) = let
          val () = D.pushStyle (ops, sty)
          val res = f ()
          val () = D.popStyle ops
          in
            res
          end
    val defaultStyle = D.defaultStyle
    fun defaultStyleMap _ = defaultStyle

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
