(* cinterface.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

local
  structure String = StringImp
in
structure CInterface :> CINTERFACE =
  struct

    type c_function = Assembly.A.c_function

    val bindCFun = Assembly.A.bind_cfun

    exception CFunNotFound of string

    fun c_function moduleName funName = let
	  val cfun = bindCFun (moduleName, funName)
	  in
	    if (InlineT.cast cfun <> 0)
	      then fn x => (Assembly.A.callc (cfun, x))
	      else raise CFunNotFound(String.concat[moduleName, ".", funName])
	  end

    type system_const = (int * string)

    exception SysConstNotFound of string

    fun findSysConst (name, l) = let
	  fun look [] = NONE
	    | look ((sysConst : system_const)::r) = if (#2 sysConst = name)
		then SOME sysConst
		else look r
	  in
	    look l
	  end

    fun bindSysConst (name, l) = (case findSysConst(name, l)
	   of (SOME sc) => sc
	    | NONE => raise(SysConstNotFound name)
	  (* end case *))

  end (* structure CInterface *)
end (* local *)
