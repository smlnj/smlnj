(* target64-object.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Implementation of the UNSAFE_OBJECT signature for 64-bit targets.
 *)

structure UnsafeObject :> UNSAFE_OBJECT =
  struct
    type object = Core.Assembly.object

  (* information about the memory representation of an object.
   * NOTE: some of these are not supported yet, but will be once the new
   * array representation is available.
   *)
    datatype representation
      = Unboxed
      | Raw
      | Raw64
      | Pair
      | Record
      | Ref
      | PolyVector
      | PolyArray	(* includes ref *)
      | ByteVector	(* includes Word8Vector.vector and CharVector.vector *)
      | ByteArray	(* includes Word8Array.array and CharArray.array *)
(*      | RealVector	use PolyVector for now *)
      | RealArray
      | Susp
      | WeakPtr

    val toObject : 'a -> object = InlineT.cast

    local
      val record1 : object -> object =
	    CInterface.c_function "SMLNJ-RunT" "record1"
      val rConcat : (object * object) -> object =
	    CInterface.c_function "SMLNJ-RunT" "recordConcat"
    in
    fun mkTuple [] = toObject()
      | mkTuple [a] = record1 a
      | mkTuple [a,b] = toObject(a,b)
      | mkTuple [a,b,c] = toObject(a,b,c)
      | mkTuple [a,b,c,d] = toObject(a,b,c,d)
      | mkTuple (a::b::c::d::r) = rConcat(toObject(a,b,c,d), mkTuple r)
    end (* local *)

    val boxed = InlineT.boxed
    val unboxed = InlineT.unboxed

    fun rep obj = if (unboxed obj)
	  then Unboxed
	  else (case (InlineT.gettag obj)
	     of 0x02 (* tag_record *) =>
		  if (InlineT.objlength obj = 2)
		    then Pair
		    else Record
	      | 0x06 (* tag_vec_hdr *) => (case (InlineT.objlength obj)
		 of 0 => PolyVector
		  | 1 => ByteVector
		  | _ => raise Fail "unknown vec_hdr"
		(* end case *))
	      | 0x0a (* tag_tag_arr_hdr *) => (case (InlineT.objlength obj)
		 of 0 => PolyArray
		  | 1 => ByteArray
		  | 6 => RealArray
		  | _ => raise Fail "unknown arr_hdr"
		(* end case *))
	      | 0x0e (* tag_arr_data/tag_ref *) =>
		  if (InlineT.objlength obj = 1)
		    then Ref
		    else raise Fail "Unknown arr_data"
	      | 0x12 (* tag_raw *) => Raw
	      | 0x16 (* tag_raw64 *) => Raw64
	      | 0x1a (* tag_special *) => (case (InlineT.getspecial obj)
		 of (0 | 1) => Susp
		  | (2 | 3) => WeakPtr
		  | _ => raise Fail "unknown special"
		(* end case *))
	      | _ (* tagless pair *) => Pair
	    (* end case *))

    exception Representation

    fun length obj = (case (rep obj)
	   of Pair => 2
	    | Unboxed => raise Representation
	    | _ => InlineT.objlength obj
	  (* end case *))

    fun nth (obj, n) = (case (rep obj)
	   of Pair =>
		if ((0 <= n) andalso (n < 2))
		  then InlineT.recordSub(obj, n)
		  else raise Representation
	    | Record => let val len = InlineT.objlength obj
		in
		  if ((0 <= n) andalso (n < len))
		    then InlineT.recordSub(obj, n)
		    else raise Representation
		end
	    | Raw64 => let val len = InlineT.objlength obj
		in
		  if ((n < 0) orelse (len <= n))
		    then raise Representation
		  else if (n = 0)
		    then obj	(* flat singleton tuple *)
		    else InlineT.cast(InlineT.raw64Sub(obj, n))
		end
	    | _ => raise Representation
	  (* end case *))

    fun toTuple obj = (case (rep obj)
	   of Unboxed => if (((InlineT.cast obj) : int) = 0)
		then []
		else raise Representation
	    | Pair => [
		  InlineT.recordSub(obj, 0),
		  InlineT.recordSub(obj, 1)
		]
	    | Record => let
		fun f i = InlineT.recordSub(obj, i)
		in
		  List.tabulate (InlineT.objlength obj, f)
		end
	    | Raw64 => let
		val len = InlineT.objlength obj
		fun f i = (InlineT.cast(InlineT.raw64Sub(obj, i)) : object)
		in
		  if (len = 1)
		    then [obj]
		    else List.tabulate (len, f)
		end
	    | _ => raise Representation
	  (* end case *))
    fun toString obj = (case (rep obj)
	   of ByteVector => ((InlineT.cast obj) : string)
	    | _ => raise Representation
	  (* end case *))
    fun toRef obj =
	  if (rep obj = Ref)
	    then ((InlineT.cast obj) : object ref)
	    else raise Representation
    fun toArray obj = (case (rep obj)
	   of PolyArray => ((InlineT.cast obj) : object array)
	    | _ => raise Representation
	  (* end case *))
    fun toRealArray obj = (case (rep obj)
	   of RealArray => ((InlineT.cast obj) : Real64Array.array)
	    | _ => raise Representation
	  (* end case *))
    fun toByteArray obj = (case (rep obj)
	   of ByteArray => ((InlineT.cast obj) : Word8Array.array)
	    | _ => raise Representation
	  (* end case *))
    fun toVector obj = (case (rep obj)
	   of PolyVector => ((InlineT.cast obj) : object vector)
	    | _ => raise Representation
	  (* end case *))
    fun toByteVector obj = (case (rep obj)
	   of ByteVector => ((InlineT.cast obj) : Word8Vector.vector)
	    | _ => raise Representation
	  (* end case *))
    fun toExn obj =
	  if ((rep obj = Record) andalso (InlineT.objlength obj = 3))
	    then ((InlineT.cast obj) : exn)
	    else raise Representation
    fun toReal64 obj = (case (rep obj)
	   of Raw64 => ((InlineT.cast obj) : Real64.real)
	    | _ => raise Representation
	  (* end case *))
    fun toInt obj = if (unboxed obj)
	  then ((InlineT.cast obj) : int)
	  else raise Representation
    fun toInt32 obj = if (unboxed obj)
	  then ((InlineT.cast obj) : Int32.int)
	  else raise Representation
    fun toInt64 obj =
	  if (rep obj = Raw) andalso (InlineT.objlength obj = 1)
	    then ((InlineT.cast obj) : Int64.int)
	    else raise Representation
    fun toWord obj = if (unboxed obj)
	  then ((InlineT.cast obj) : word)
	  else raise Representation
    fun toWord8 obj = if (unboxed obj)
	  then ((InlineT.cast obj) : Word8.word)
	  else raise Representation
    fun toWord32 obj = if (unboxed obj)
	  then ((InlineT.cast obj) : Word32.word)
	  else raise Representation
    fun toWord64 obj =
	  if (rep obj = Raw) andalso (InlineT.objlength obj = 1)
	    then ((InlineT.cast obj) : Word64.word)
	    else raise Representation

  end;
