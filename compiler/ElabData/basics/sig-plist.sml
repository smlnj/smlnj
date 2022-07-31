(* sig-plist.sml *)

structure SigPropList =
struct

  val { getFn = sigBoundeps, setFn = setSigBoundeps, ... } = let
      fun holder (e: Modules.sigrec) = #properties e
      fun init _ = NONE: (EntPath.entPath * TKind.tkind) list option
  in
      PropList.newProp (holder, init)
  end

end (* structure SigPropList *)
