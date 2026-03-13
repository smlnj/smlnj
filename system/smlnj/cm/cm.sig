(*
 * This is the signature for a "full" structure CM.  This structure gets
 * constructed in cm-boot.sml and is made available at top-level by
 * (auto-)loading the library "full-cm.cm".
 * (After system startup only a "minimal" structure CM is visible.)
 *
 *   Copyright (c) 1999 by Lucent Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
signature CM = sig

    val autoload : string -> bool
    val make : string -> bool
    val recomp : string -> bool
    val stabilize : bool -> string -> bool

    type 'a controller = { get : unit -> 'a, set : 'a -> unit }

    structure Anchor : sig
	val anchor : string -> string option controller
	val reset : unit -> unit
    end

    structure Control : sig
	val keep_going : bool controller
	val verbose : bool controller
	val parse_caching : int controller
	val warn_obsolete : bool controller
	val debug : bool controller
	val conserve_memory : bool controller
	val generate_index : bool controller
    end

    structure Library : sig
	type lib
	val known : unit -> lib list
	val descr : lib -> string
	val osstring : lib -> string
	val dismiss : lib -> unit
	val unshare : lib -> unit
    end

    structure State : sig
	val synchronize : unit -> unit
	val reset : unit -> unit
	val pending : unit -> string list
	val showBindings : unit -> unit
    end

    structure Server : sig
	type server
	val start : { cmd : string * string list,
		      name : string,
		      pathtrans : (string -> string) option,
		      pref : int } -> server option
	val stop : server -> unit
	val kill : server -> unit
	val name : server -> string
    end

    val sources :
	{ arch: string, os: string } option ->
	string -> { file: string, class: string, derived: bool } list option

    val symval : string -> int option controller
    val load_plugin : string -> bool

    val cm_dir_arc : string

    val mk_standalone : bool option ->
			{ setup: string option,
			  project: string,
			  wrapper: string,
			  target: string } ->
			string list option

    structure Graph : sig
	val graph : string -> { graph: PortableGraph.graph,
				imports: Library.lib list,
				nativesrc: string -> string } option
    end

    val redump_heap : string -> unit
end
