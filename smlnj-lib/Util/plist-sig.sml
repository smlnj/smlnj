(* plist-sig.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Property lists using Stephen Weeks's implementation.
 *)

signature PROP_LIST =
  sig

    type holder

    val newHolder : unit -> holder

    val hasProps : holder -> bool
	(* return true if the holder has any properties. *)

    val clearHolder : holder -> unit
	(* remove all properties and flags from the holder *)

    val sameHolder : (holder * holder) -> bool
	(* returns true, if two holders are the same *)

  (* newProp (selHolder, init)
   * creates a new property for objects of type 'a and returns
   * functions to get the property, set it, and clear it.  The function
   * selHolder is used to select the holder field from an object
   * and init is used to create the initial property value.
   * Typically, properties are reference cells, so that they can
   * be modified.  The difference between peekFn and getFn is that
   * peekFn returns NONE when the property has not yet been created,
   * whereas getFn will allocate and initialize the property.  The
   * setFn function can either be used to initialize an undefined property
   * or to override a property's current value.
   *)
    val newProp : (('a -> holder) * ('a -> 'b)) -> {
	    peekFn : 'a -> 'b option,
	    getFn  : 'a -> 'b,
	    setFn  : ('a * 'b) -> unit,
	    clrFn  : 'a -> unit
	  }

    val newFlag : ('a -> holder) -> {
	    getFn : 'a -> bool,
	    setFn : ('a * bool) -> unit
	  }

  end

