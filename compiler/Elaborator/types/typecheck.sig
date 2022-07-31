(* typecheck.sig
 *
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature TYPECHECK =
sig

  val decType : StaticEnv.staticEnv * Absyn.dec * bool
		* ErrorMsg.errorFn * SourceMap.region -> Absyn.dec
  val debugging : bool ref

end (* signature TYPECHECK *)
