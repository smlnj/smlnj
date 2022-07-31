(* dynenv.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)
structure DynamicEnv : DYNAMICENV =
    PidEnvFn (type binding = Unsafe.Object.object)
