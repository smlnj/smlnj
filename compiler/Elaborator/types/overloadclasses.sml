(* overloadclasses.sml 
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure OverloadClasses =
struct

local structure BT = BasicTypes in

  (* overloading class *)
  type class = Types.ty list

(* overload classes *)
    val intClass = [BT.intTy,
		    BT.int32Ty,
		    BT.int64Ty,
		    BT.intinfTy]

    val wordClass = [BT.wordTy,
		     BT.word8Ty,
		     BT.word32Ty,
		     BT.word64Ty]

    val int_wordClass = intClass @ wordClass

    val realClass = [BT.realTy]

    val	int_realClass = intClass @ realClass

    val numClass = int_wordClass @ realClass

    val textClass = [BT.charTy, BT.stringTy]

    val num_textClass = numClass @ textClass

    fun inClass (ty: Types.ty, class: class) =
	List.exists (fn ty' => TypesUtil.equalType(ty', ty)) class

(* Note: realClass and textClass may be expanded in the future
*  when new overloaded operations over multiple real and text
*  types are added. *)

end (* local *)
end (* structure OverloadClasses *)
