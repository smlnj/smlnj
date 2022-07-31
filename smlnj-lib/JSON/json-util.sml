(* json-util.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Utility functions for processing the JSON in-memory representation.
 *)

structure JSONUtil : sig

  (* exceptions for conversion functions *)
    exception NotBool of JSON.value
    exception NotInt of JSON.value
    exception NotNumber of JSON.value
    exception NotString of JSON.value

  (* exception that is raised when trying to process a non-object value as an object *)
    exception NotObject of JSON.value

  (* exception that is raised when the given field is not found in an object *)
    exception FieldNotFound of JSON.value * string

  (* exception that is raised when trying to process a non-array value as an array *)
    exception NotArray of JSON.value

  (* exception that is raised when access to an array value is out of bounds *)
    exception ArrayBounds of JSON.value * int

  (* exception that is raised when a `FIND` edge does not match any array element *)
    exception ElemNotFound of JSON.value

  (* map the above exceptions to a message string; we use General.exnMessage for other
   * exceptions.
   *)
    val exnMessage : exn -> string

  (* conversion functions for atomic values.  These raise the corresponding
   * "NotXXX" exceptions when their argument has the wrong shape.  Also note
   * that asNumber will accept both integers and floats and asInt may raise
   * Overflow if the number is too large.
   *)
    val asBool : JSON.value -> bool
    val asInt : JSON.value -> Int.int
    val asIntInf : JSON.value -> IntInf.int
    val asNumber : JSON.value -> Real.real
    val asString : JSON.value -> string

  (* find a field in an object; this function raises the NotObject exception when
   * the supplied value is not an object.
   *)
    val findField : JSON.value -> string -> JSON.value option

  (* lookup a field in an object; this function raises the NotObject exception when
   * the supplied value is not an object and raises FieldNotFound if the value is
   * an object, but does not have the specified field.
   *)
    val lookupField : JSON.value -> string -> JSON.value

  (* does a JSON object have a given field?  This function returns false if called
   * on a non-object value.
   *)
    val hasField : string -> JSON.value -> bool

  (* does the specified field of an object satisfy the given predicate?  This function
   * returns false if called on a non-object value.
   *)
    val testField : string -> (JSON.value -> bool) -> JSON.value -> bool

  (* convert a JSON array to an SML vector *)
    val asArray : JSON.value -> JSON.value vector

  (* map a conversion function over a JSON array to produce a list; this function
   * raises the NotArray exception if the second argument is not an array.
   *)
    val arrayMap : (JSON.value -> 'a) -> JSON.value -> 'a list

  (* path specification for indexing into JSON values *)
    datatype edge
      = SEL of string   (* select field of object *)
      | SUB of int      (* index into array component *)
      | FIND of JSON.value -> bool
                        (* first array component that satisfies the predicate *)

    type path = edge list

  (* `get (jv, path)` returns the component of `jv` named by `path`.  It raises
   * the NotObject, NotArray, FieldNotFound, and ArrayBounds exceptions if there
   * is an inconsistency between the path and the structure of `jv`.
   *)
    val get : JSON.value * path -> JSON.value

  (* `replace (jv, path, v)` replaces the component of `jv` named by `path`
   * with the value `v`.
   *)
    val replace : JSON.value * path * JSON.value -> JSON.value

  (* `insert (jv, path, lab, v)` inserts `lab : v` into the object named by `path`
   * in `jv`
   *)
    val insert : JSON.value * path * string * JSON.value -> JSON.value

  (* `append (jv, path, vs)` appends the list of values `vs` onto the array named by `path`
   * in `jv`
   *)
    val append : JSON.value * path * JSON.value list -> JSON.value

  end = struct

    structure J = JSON

    exception NotBool of J.value
    exception NotInt of J.value
    exception NotNumber of J.value
    exception NotString of J.value

    exception NotObject of J.value
    exception FieldNotFound of J.value * string

    exception NotArray of J.value
    exception ArrayBounds of J.value * int
    exception ElemNotFound of JSON.value

  (* conversion functions for atomic values *)
    fun asBool (J.BOOL b) = b
      | asBool v = raise NotBool v

    fun asInt (J.INT n) = Int.fromLarge n
      | asInt v = raise NotInt v

    fun asIntInf (J.INT n) = n
      | asIntInf v = raise NotInt v

    fun asNumber (J.INT n) = Real.fromLargeInt n
      | asNumber (J.FLOAT f) = f
      | asNumber v = raise NotNumber v

    fun asString (J.STRING s) = s
      | asString v = raise NotString v

    fun findField (J.OBJECT fields) = let
	  fun find lab = (case List.find (fn (l, v) => (l = lab)) fields
		 of NONE => NONE
		  | SOME(_, v) => SOME v
		(* end case *))
	  in
	    find
	  end
      | findField v = raise NotObject v

    fun lookupField (v as J.OBJECT fields) = let
	  fun find lab = (case List.find (fn (l, v) => (l = lab)) fields
		 of NONE => raise FieldNotFound(v, concat["no definition for field \"", lab, "\""])
		  | SOME(_, v) => v
		(* end case *))
	  in
	    find
	  end
      | lookupField v = raise NotObject v

    fun hasField lab (J.OBJECT fields) = List.exists (fn (lab', _) => lab = lab') fields
      | hasField _ _ = false

    fun testField lab pred (J.OBJECT fields) = (
	  case List.find (fn (lab', _) => lab = lab') fields
	   of SOME(_, v) => pred v
	    | NONE => false
	  (* end case *))
      | testField _ _ _ = false

    fun asArray (J.ARRAY vs) = Vector.fromList vs
      | asArray v = raise NotArray v

    fun arrayMap f (J.ARRAY vs) = List.map f vs
      | arrayMap f v = raise NotArray v

  (* map the above exceptions to a message string; we use General.exnMessage for other
   * exceptions.
   *)
    fun exnMessage exn = let
	  fun v2s (J.ARRAY _) = "array"
	    | v2s (J.BOOL false) = "'false'"
	    | v2s (J.BOOL true) = "'true'"
	    | v2s (J.FLOAT _) = "number"
	    | v2s (J.INT _) = "number"
	    | v2s J.NULL = "'null'"
	    | v2s (J.OBJECT _) = "object"
	    | v2s (J.STRING _) = "string"
	  in
	    case exn
	     of NotBool v => String.concat[
		    "expected boolean, but found ", v2s v
		  ]
	      | NotInt(J.FLOAT _) => "expected integer, but found floating-point number"
	      | NotInt v => String.concat[
		    "expected integer, but found ", v2s v
		  ]
	      | NotNumber v => String.concat[
		    "expected number, but found ", v2s v
		  ]
	      | NotString v => String.concat[
		    "expected string, but found ", v2s v
		  ]
	      | NotObject v => String.concat[
		    "expected object, but found ", v2s v
		  ]
	      | FieldNotFound(v, fld) => String.concat[
		    "no definition for field \"", fld, "\" in object"
		  ]
	      | NotArray v => String.concat[
		    "expected array, but found ", v2s v
		  ]
	      | ElemNotFound v => String.concat[
		    "no matching element found in ", v2s v
		  ]
	      | _ => General.exnMessage exn
	    (* end case *)
	  end

  (* path specification for indexing into JSON values *)
    datatype edge
      = SEL of string   (* select field of object *)
      | SUB of int      (* index into array component *)
      | FIND of JSON.value -> bool
                        (* first array component that satisfies the predicate *)

    type path = edge list

    fun get (v, []) = v
      | get (v as J.OBJECT fields, SEL lab :: rest) =
	  (case List.find (fn (l, v) => (l = lab)) fields
	   of NONE => raise FieldNotFound(v, lab)
	    | SOME(_, v) => get (v, rest)
	  (* end case *))
      | get (v, SEL _ :: _) = raise NotObject v
      | get (v as J.ARRAY vs, SUB i :: rest) = let
	  fun nth ([], _) = raise ArrayBounds(v, i)
	    | nth (elem::_, 0) = elem
	    | nth (_::r, i) = nth(r, i-1)
	  in
	    if (i < 0)
	      then raise ArrayBounds(v, i)
	      else get (nth(vs, i), rest)
	  end
      | get (v, SUB _ :: _) = raise (NotArray v)
      | get (v as J.ARRAY vs, FIND pred :: rest) = (case List.find pred vs
	   of NONE => raise ElemNotFound v
	    | SOME v => get (v, rest)
	  (* end case *))
      | get (v, FIND _ :: _) = raise (NotArray v)

  (* top-down zipper to support functional editing *)
    datatype zipper
      = ZNIL
      | ZOBJ of {
            prefix : (string * J.value) list,
            label : string,
            child : zipper,
            suffix : (string * J.value) list
          }
      | ZARR of {
            prefix : J.value list,
            child : zipper,
            suffix : J.value list
          }

  (* follow a path into a JSON value while constructing a zipper *)
    fun unzip (v, []) = (ZNIL, v)
      | unzip (v as J.OBJECT fields, SEL lab :: rest) = let
          fun find (_, []) = raise FieldNotFound(v, lab)
            | find (pre, (l, v)::flds) = if (l = lab)
                then let
		  val (zipper, v) = unzip (v, rest)
		  in
		    (ZOBJ{prefix=pre, label=lab, suffix=flds, child=zipper}, v)
                  end
                else find ((l, v)::pre, flds)
          in
            find ([], fields)
          end
      | unzip (v, SEL _ :: _) = raise NotObject v
      | unzip (v as J.ARRAY vs, SUB i :: rest) = let
          fun sub (_, [], _) = raise ArrayBounds(v, i)
            | sub (prefix, v::vs, 0) = let
		val (zipper, v) = unzip (v, rest)
		in
		  (ZARR{prefix = prefix, child = zipper, suffix = vs}, v)
		end
            | sub (prefix, v::vs, i) = sub (v::prefix, vs, i-1)
	  in
	    sub ([], vs, i)
	  end
      | unzip (v, SUB _ :: _) = raise NotArray v
      | unzip (v as J.ARRAY vs, FIND pred :: rest) = let
          fun find (_, []) = raise ElemNotFound v
            | find (prefix, v::vs) = if pred v
                then let
                  val (zipper, v) = unzip (v, rest)
                  in
                    (ZARR{prefix = prefix, child = zipper, suffix = vs}, v)
                  end
                else find (v::prefix, vs)
          in
            find ([], vs)
          end
      | unzip (v, FIND _ :: _) = raise (NotArray v)

  (* zip up a zipper *)
    fun zip (zipper, v) = let
	  fun zip' ZNIL = v
            | zip' (ZOBJ{prefix, label, suffix, child}) =
                J.OBJECT(List.revAppend(prefix, (label, zip' child)::suffix))
            | zip' (ZARR{prefix, child, suffix}) =
                J.ARRAY(List.revAppend(prefix, zip' child :: suffix))
          in
	    zip' zipper
	  end

    fun replace (jv, path, v) = zip (#1 (unzip (jv, path)), v)

    fun insert (jv, path, label, v) = (case unzip (jv, path)
	   of (zipper, J.OBJECT fields) => zip (zipper, J.OBJECT((label, v)::fields))
	    | (_, v) => raise NotObject v
	  (* end case *))

    fun append (jv, path, vs) = (case unzip (jv, path)
	   of (zipper, J.ARRAY jvs) => zip (zipper, J.ARRAY(jvs @ vs))
	    | (_, v) => raise NotArray v
	  (* end case *))

  end
