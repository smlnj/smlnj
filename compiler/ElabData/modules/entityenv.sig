(* Copyright 1996 by AT&T Bell Laboratories *)
(* entityenv.sig *)

signature ENTITY_ENV =
sig

  type entVar = EntPath.entVar
  type entPath = EntPath.entPath
  type entityEnv = Modules.entityEnv

  exception Unbound

  val empty : entityEnv
  val mark : ((unit->Stamps.stamp) * entityEnv) -> entityEnv
  val bind : entVar * Modules.entity * entityEnv -> entityEnv
  val atop : entityEnv * entityEnv -> entityEnv
  val atopSp : entityEnv * entityEnv -> entityEnv

  val toList : entityEnv -> (entVar * Modules.entity) list

  val look : entityEnv * entVar -> Modules.entity
  val lookStrEnt : entityEnv * entVar -> Modules.strEntity
  val lookTycEnt : entityEnv * entVar -> Modules.tycEntity
  val lookFctEnt : entityEnv * entVar -> Modules.fctEntity

  val lookEP : entityEnv * entPath -> Modules.entity
  val lookTycEP : entityEnv * entPath -> Modules.tycEntity 
  val lookStrEP : entityEnv * entPath -> Modules.strEntity 
  val lookFctEP : entityEnv * entPath -> Modules.fctEntity 

  val debugging : bool ref

end (* signature ENTITY_ENV *)

