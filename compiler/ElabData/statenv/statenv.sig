(* statenv.sig
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)
signature STATICENV =
sig

    (* Static environments now optionally contain modtrees anchored at
     * bindings.  This allows for rapid on-demand construction of
     * modmaps (= pickling/unpickling contexts).
     *
     * March 2000, Matthias Blume  *)
  type staticEnv
  type binding = Bindings.binding
  type real_binding = binding * Modules.modtree option

  exception Unbound

  val empty: staticEnv
  val look: staticEnv * Symbol.symbol -> binding
  val bind: Symbol.symbol * binding * staticEnv -> staticEnv
  val bindRB: Symbol.symbol * real_binding * staticEnv -> staticEnv

  val special: (Symbol.symbol -> binding) * (unit -> Symbol.symbol list)
                  -> staticEnv

  val atop: staticEnv * staticEnv -> staticEnv
  val consolidate: staticEnv -> staticEnv
  val consolidateLazy: staticEnv -> staticEnv
  val app: (Symbol.symbol * binding -> unit) -> staticEnv -> unit
  val map: (binding -> binding) -> staticEnv -> staticEnv
  val fold: ((Symbol.symbol * binding) * 'a -> 'a) -> 'a -> staticEnv -> 'a
  val realfold : ((Symbol.symbol * real_binding) * 'a -> 'a) -> 'a -> staticEnv -> 'a
  val foldOverElems: ((Symbol.symbol * binding) * 'a -> 'a) * 'a * staticEnv * Symbol.symbol list -> 'a
  val sort: staticEnv -> (Symbol.symbol * binding) list

  val symbols : staticEnv -> Symbol.symbol list


  val filter : staticEnv * Symbol.symbol list -> staticEnv

end (* signature STATICENV *)
