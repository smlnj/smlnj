structure X86CSizes =
  struct

    structure Ty = CType

  (* align the address to the given alignment, which must be a power of 2 *)
    fun alignAddr (addr, align) = let
	  val mask = Word.fromInt(align-1)
	  in
	    Word.toIntX(Word.andb(Word.fromInt addr + mask, Word.notb mask))
	  end

    fun align4 addr = Word.toIntX(Word.andb(Word.fromInt addr + 0w3, Word.notb 0w3))

  (* size and natural alignment for integer types. *)
    fun sizeOfInt Ty.I_char = {ty = 8, sz = 1, align = 1}
      | sizeOfInt Ty.I_short = {ty = 16, sz = 2, align = 2}
      | sizeOfInt Ty.I_int = {ty = 32, sz = 4, align = 4}
      | sizeOfInt Ty.I_long = {ty = 32, sz = 4, align = 4}
      | sizeOfInt Ty.I_long_long = {ty = 64, sz = 8, align = 4}

  (* sizes of other C types *)
    val sizeOfPtr = {ty = 32, sz = 4, align = 4}

  (* compute the size and alignment information for a struct; tys is the list
   * of member types.
   * The total size is padded to agree with the struct's alignment.
   *)
    fun sizeOfStruct tys = let
	  fun ssz ([], maxAlign, offset) =
		{sz = alignAddr(offset, maxAlign), align = maxAlign}
	    | ssz (ty::tys, maxAlign, offset) = let
		  val {sz, align} = sizeOfTy ty
		  val offset = alignAddr(offset, align)
		  in
		    ssz (tys, Int.max(maxAlign, align), offset+sz)
		  end
	  in
	    ssz (tys, 1, 0)
	  end

  (* the size alignment of a union type is the maximum of the sizes and alignments of the
   * members.  The final size is padded to agree with the alignment.
   *)
    and sizeOfUnion tys = let
	  fun usz ([], maxAlign, maxSz) =
		{sz = alignAddr(maxSz, maxAlign), align = maxAlign}
	    | usz (ty::tys, maxAlign, maxSz) = let
		  val {sz, align} = sizeOfTy ty
		  in
		    usz (tys, Int.max(maxAlign, align), Int.max(sz, maxSz))
		  end
	  in
	    usz (tys, 1, 0)
	  end

    and sizeOfTy Ty.C_void = raise Fail "unexpected void argument type"
      | sizeOfTy Ty.C_float = {sz = 4, align = 4}
      | sizeOfTy Ty.C_double = {sz = 8, align = 4}
      | sizeOfTy Ty.C_long_double = {sz = 12, align = 4}
      | sizeOfTy (Ty.C_unsigned isz) = let
	  val {sz, align, ...} = sizeOfInt isz
	  in
	    {sz = sz, align = align}
	  end
      | sizeOfTy (Ty.C_signed isz) = let
	  val {sz, align, ...} = sizeOfInt isz
	  in
	    {sz = sz, align = align}
	  end
      | sizeOfTy Ty.C_PTR = {sz = 4, align = 4}
      | sizeOfTy (Ty.C_ARRAY(ty, n)) = let
	  val {sz, align} = sizeOfTy ty
	  in
	    {sz = n*sz, align = align}
	  end
      | sizeOfTy (Ty.C_STRUCT tys) = sizeOfStruct tys
      | sizeOfTy (Ty.C_UNION tys) = sizeOfUnion tys

  end (* IA32CSizes *)
