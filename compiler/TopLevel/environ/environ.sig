(* environ.sig
 *
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature ENVIRONMENT =
sig

  type staticEnv
  type dynenv
  type environment
       (* = { static: staticEnv, dynamic: dynenv } *)
  type symbol (* = Symbol.symbol *)

  val emptyEnv : environment
  val staticPart : environment -> staticEnv
  val dynamicPart : environment -> dynenv
  val mkenv : { static: staticEnv, dynamic: dynenv } -> environment

  val layerEnv    : environment * environment -> environment
  val concatEnv   : environment * environment -> environment
  val layerStatic : staticEnv * staticEnv -> staticEnv
  val filterEnv   : environment * Symbol.symbol list -> environment
(*
  val filterStaticEnv : staticEnv * Symbol.symbol list -> staticEnv
*)
  val consolidateEnv : environment -> environment
  val consolidateStatic : staticEnv -> staticEnv

  (* reduce dynamic and symbolic part to what's actually needed *)
  val trimEnv: environment -> environment

  val describe : staticEnv -> Symbol.symbol -> unit

  val primEnv : staticEnv

end (* signature ENVIRONMENT *)


