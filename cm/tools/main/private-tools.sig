(*
 * A private interface to CM's tools mechanism to be used internally
 * by CM itself.
 *
 *   (C) 2006 The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)
signature PRIVATETOOLS = sig
    include CORETOOLS where type srcpath = SrcPath.file
		      where type presrcpath = SrcPath.prefile

    type registry

    val newRegistry : unit -> registry

    val expand : { error: string -> unit,
		   local_registry : registry,
		   spec: spec,
		   context: SrcPath.dir,
		   load_plugin: SrcPath.dir -> string -> bool,
		   sysinfo: { symval: string -> int option,
			      archos: string } }
	-> expansion

    val withPlugin : SrcPath.file -> (unit -> 'a) -> 'a
end
