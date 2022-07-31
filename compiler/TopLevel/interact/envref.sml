(* COPYRIGHT (c) 1996 Bell Laboratories.*)
(* envref.sml *)

signature ENVREF = sig
  type environment = Environment.environment
  type envref = {get: unit -> environment, set: environment -> unit}
  type envstate = { loc: envref, base: envref, props: PropList.holder }

  val state : unit -> envstate

  val loc : unit -> envref		(* interactive top level env *)
  val base : unit -> envref
  val props : unit -> PropList.holder

  val pervasive : envref

  val combined : unit -> environment

  (* push a given envstate onto the stack, run the thunk, then pop the state *)
  val locally : envstate * (unit -> 'a) -> 'a

  val listBoundSymbols : unit -> Symbol.symbol list
end

structure EnvRef : ENVREF = struct
    type environment = Environment.environment
    type envref = {get: unit -> environment, set: environment -> unit}
    type envstate = { loc: envref, base: envref, props: PropList.holder }

    fun mkEnvRef a = let
	val r = ref a
	fun get () = !r
	fun set x = r := x
    in
	{ get = get, set = set }
    end

    val pervasive = mkEnvRef Environment.emptyEnv

    val stack : (envstate * envstate list) ref = let
	val loc = mkEnvRef Environment.emptyEnv
	val props = PropList.newHolder ()
    in
	ref ({ loc = loc, base = pervasive, props = props }, [])
    end

    fun state () = #1 (!stack)
    val loc = #loc o state
    val base = #base o state
    val props = #props o state

    fun combined () =
	Environment.layerEnv (#get (loc ()) (),
			      #get (base ()) ())

    fun locally (es, th) = let
	val oldstack = !stack
    in
	stack := (es, op :: oldstack);
	th ()
	before stack := oldstack
    end

    fun listBoundSymbols () =
	StaticEnv.symbols
	    (StaticEnv.atop (#static (#get (loc ()) ()),
			     #static (#get (base ()) ())))
end
