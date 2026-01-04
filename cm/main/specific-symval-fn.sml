(* specific-symval-fn.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * Building a host/OS-specific environments for CM "preprocessor" variables.
 *
 * Author: Matthias Blume
 *)

functor SpecificSymValFn (

    val arch : string
    val os : SMLofNJ.SysInfo.os_kind

  ) : sig

    val symval : string -> {
            get : unit -> int option,
            set : int option -> unit
          }

  end = struct

    val (arch, big, size) = (case arch
           of "amd64" => ("AMD64", false, 64)
             | "arm64" => ("ARM64", false, 64)
             | arch => ErrorMsg.impossible ("unknown architecture: " ^ arch)
          (* end case *))

    val env0 = SymVal.default {
            arch = arch, big = big, size = size, os = os,
            version = #version_id SMLNJVersion.version,
            extra_syms = []
          }

    val er = ref env0

    fun symval s = let
          fun get () = SymVal.look (!er) s
          fun set v = er := SymVal.define (!er, s, v)
          in
            { get = get, set = set }
          end

    end
