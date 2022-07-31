(* posix-error.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Structure for POSIX error codes.
 *)

local
  structure SysWord = SysWordImp
in
structure POSIX_Error =
  struct

    type syserror = int (* = PreBasis.syserror *)

    fun cfun x = CInterface.c_function "POSIX-Error" x

    val errors : CInterface.system_const list = cfun "listerrors" ()

    fun osVal s = #1(CInterface.bindSysConst(s, errors))

    fun syserror s = (case CInterface.findSysConst(s, errors)
	   of NONE => NONE
	    | (SOME(e, _)) => SOME e
	  (* end case *))

    val errmsg : int -> string = cfun "errmsg"
    val geterror : int -> CInterface.system_const = cfun "geterror"

    fun toWord i = SysWord.fromInt i
    fun fromWord w = SysWord.toInt w
    fun errorMsg i = errmsg i
    fun errorName err = #2(geterror err)

    val toobig      = osVal "toobig"
    val acces       = osVal "acces"
    val again       = osVal "again"
    val badf        = osVal "badf"
    val badmsg      = osVal "badmsg"
    val busy        = osVal "busy"
    val canceled    = osVal "canceled"
    val child       = osVal "child"
    val deadlk      = osVal "deadlk"
    val dom         = osVal "dom"
    val exist       = osVal "exist"
    val fault       = osVal "fault"
    val fbig        = osVal "fbig"
    val inprogress  = osVal "inprogress"
    val intr        = osVal "intr"
    val inval       = osVal "inval"
    val io          = osVal "io"
    val isdir       = osVal "isdir"
    val loop        = osVal "loop"
    val mfile       = osVal "mfile"
    val mlink       = osVal "mlink"
    val msgsize     = osVal "msgsize"
    val nametoolong = osVal "nametoolong"
    val nfile       = osVal "nfile"
    val nodev       = osVal "nodev"
    val noent       = osVal "noent"
    val noexec      = osVal "noexec"
    val nolck       = osVal "nolck"
    val nomem       = osVal "nomem"
    val nospc       = osVal "nospc"
    val nosys       = osVal "nosys"
    val notdir      = osVal "notdir"
    val notempty    = osVal "notempty"
    val notsup      = osVal "notsup"
    val notty       = osVal "notty"
    val nxio        = osVal "nxio"
    val perm        = osVal "perm"
    val pipe        = osVal "pipe"
    val range       = osVal "range"
    val rofs        = osVal "rofs"
    val spipe       = osVal "spipe"
    val srch        = osVal "srch"
    val xdev        = osVal "xdev"

  end (* structure POSIX_Error *)

end (* local *)

