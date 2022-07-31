(* moduleutil.sig
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature MODULEUTIL =
sig

exception Unbound of Symbol.symbol

val getSpec : Modules.elements * Symbol.symbol -> Modules.spec
val getSpecVar : Modules.spec -> EntPath.entVar option

val strDefToStr : Modules.strDef * Modules.entityEnv -> Modules.Structure

(*** getTyc, getStr and getFct are used in modules/sigmatch.sml only ***)
val getTyc : Modules.elements * Modules.entityEnv * Symbol.symbol
                 -> Types.tycon * EntPath.entVar

val getStr : Modules.elements * Modules.entityEnv
	     * Symbol.symbol * Access.access * PrimopId.str_prim_info
             -> Modules.Structure * EntPath.entVar

val getFct : Modules.elements * Modules.entityEnv
             * Symbol.symbol * Access.access * PrimopId.str_prim_info
             -> Modules.Functor * EntPath.entVar

(*** these functions are used in eqtypes.sml ***)
val getStrStamp : Modules.Structure -> Stamps.stamp
val getStrName : Modules.Structure -> InvPath.path
val getStrs : Modules.Structure -> Modules.Structure list
val getTycs : Modules.Structure -> Types.tycon list
val getStrSymbols : Modules.Structure -> Symbol.symbol list

(*** these functions should be called in env/lookup.sml only ***)
val getStrPath : Modules.Structure * SymPath.path * SymPath.path
                 -> Modules.Structure

val getStrDef : Modules.Structure * SymPath.path * SymPath.path
                -> Modules.strDef

val getFctPath : Modules.Structure * SymPath.path * SymPath.path
                 -> Modules.Functor
val getTycPath : Modules.Structure * SymPath.path * SymPath.path
                 -> Types.tycon
val getValPath : Modules.Structure * SymPath.path * SymPath.path
                 -> Absyn.value

val checkPathSig : Modules.Signature * SymPath.path
		   -> Symbol.symbol option

val eqSign : Modules.Signature * Modules.Signature -> bool
val eqOrigin : Modules.Structure * Modules.Structure -> bool

val tycId : Types.tycon -> ModuleId.tycId
val strId: Modules.Structure -> ModuleId.strId
val strId2: Modules.Signature * Modules.strEntity -> ModuleId.strId
val fctId: Modules.Functor -> ModuleId.fctId
val fctId2: Modules.fctSig * Modules.fctEntity -> ModuleId.fctId

(*** translate tycon or type in an entityEnv ***)
val transTycon : Modules.entityEnv -> Types.tycon -> Types.tycon
val transType : Modules.entityEnv -> Types.ty -> Types.ty

(*** relativize type or tycon in an epcontext ***)
val relativizeTyc : EntPathContext.context -> Types.tycon -> Types.tycon * bool
val relativizeType : EntPathContext.context -> Types.ty -> Types.ty * bool

val openStructure : StaticEnv.staticEnv * Modules.Structure
		    -> StaticEnv.staticEnv

(*** extract inl_info from a list of bindings *)
val strPrimElemInBinds : Bindings.binding list -> PrimopId.str_prim_info

val getElementsSymbols : Modules.elements -> Symbol.symbol list
val getSigSymbols: Modules.Signature -> Symbol.symbol list

val getSignatureNames : Modules.Structure -> Symbol.symbol list

val debugging : bool ref

end (* signature MODULEUTIL *)
