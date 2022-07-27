(* ident-fn.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * A functor for generating the various kinds of identifiers used in the AST.
 *)

signature IDENTIFIER =
  sig

    type t

    val new : Atom.atom -> t

    val atomOf : t -> Atom.atom
    val nameOf : t -> string

    val same : t * t -> bool
    val compare : t * t -> order
    val hash : t -> word

    val newProp : (t -> 'a) -> {
	    clrFn : t -> unit,
	    getFn : t -> 'a,
	    peekFn : t -> 'a option,
	    setFn : t * 'a -> unit
	  }
    val newFlag : unit -> {
	    getFn : t -> bool,
	    setFn : t * bool -> unit
	  }

  end


functor IdentFn () : IDENTIFIER =
  struct

    datatype t = ID of {
	id : Atom.atom,
	stamp : word,
	props : PropList.holder
      }

    local
      val cnt = ref 0w0
    in
    fun new id = let
	  val stamp = !cnt
	  in
	    cnt := stamp + 0w1;
	    ID{id = id, stamp = stamp, props = PropList.newHolder()}
	  end
    end (* local *)

    fun atomOf (ID{id, ...}) = id
    fun nameOf (ID{id, ...}) = Atom.toString id

    fun same (ID{stamp=a, ...}, ID{stamp=b, ...}) = (a = b)

    fun compare (ID{stamp=a, ...}, ID{stamp=b, ...}) = Word.compare(a, b)

    fun hash (ID{stamp, ...}) = stamp

    local
      fun getHolder (ID{props, ...}) = props
    in
    fun newProp initFn = PropList.newProp (getHolder, initFn)
    fun newFlag () = PropList.newFlag getHolder
    end (* local *)

  end (* functor IdentFn *)
