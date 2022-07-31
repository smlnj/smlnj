structure SparcCSizes =
  struct

    structure Ty = CType

    fun roundup (i, a) = a * ((i + a - 1) div a)

  (* calculate size and alignment for a C type *)
    fun sizeOfTy (Ty.C_void | Ty.C_float | Ty.C_PTR |
	      Ty.C_signed (Ty.I_int | Ty.I_long) |
	      Ty.C_unsigned (Ty.I_int | Ty.I_long)) = {sz=4, align=4}
      | sizeOfTy (Ty.C_double |
	      Ty.C_signed Ty.I_long_long |
	      Ty.C_unsigned Ty.I_long_long) = {sz=8, align=8}
      | sizeOfTy (Ty.C_long_double) = {sz=16, align=8}
      | sizeOfTy (Ty.C_signed Ty.I_char | Ty.C_unsigned Ty.I_char) = {sz=1, align=1}
      | sizeOfTy (Ty.C_signed Ty.I_short | Ty.C_unsigned Ty.I_short) = {sz=2, align=2}
      | sizeOfTy (Ty.C_ARRAY (t, n)) = let val {sz=s, align=a} = sizeOfTy t in {sz=n * s, align=a} end
      | sizeOfTy (Ty.C_STRUCT l) =
	let (* i: next free memory address (relative to struct start);
	     * a: current total alignment,
	     * l: list of struct member types *)
	    fun pack (i, a, []) =
		(* when we are done with all elements, the total size
		 * of the struct must be padded out to its own alignment *)
		{sz=roundup (i, a), align=a}
	      | pack (i, a, t :: tl) = let
		    val {sz=ts, align=ta} = sizeOfTy t (* size and alignment for member *)
		in
		    (* member must be aligned according to its own
		     * alignment requirement; the next free position
		     * is then at "aligned member-address plus member-size";
		     * new total alignment is max of current alignment
		     * and member alignment (assuming all alignments are
		     * powers of 2) *)
		    pack (roundup (i, ta) + ts, Int.max (a, ta), tl)
		end
	in
	    pack (0, 1, l)
	end
      | sizeOfTy (Ty.C_UNION l) =
	let (* m: current max size
	     * a: current total alignment *)
	    fun overlay (m, a, []) = {sz=roundup (m, a), align=a}
	      | overlay (m, a, t :: tl) =
		let val {sz=ts, align=ta} = sizeOfTy t
		in
		    overlay (Int.max (m, ts), Int.max (a, ta), tl)
		end
	in
	    overlay (0, 1, l)
	end

  end
