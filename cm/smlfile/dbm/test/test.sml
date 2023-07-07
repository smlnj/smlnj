(* A.sml *)

(* Ast for:

  structure A =
  struct
    structure AA = struct val x = 3 end
  end

  structure B =
  struct
    open A
    val y = AA.x
  end

 *)
structure Test =
struct	      

local
  open Ast
in

(* structure A *)

val vb_x = Vb {pat=VarPat ["x"], exp=IntExp "3", lazyp=false}

val dec_x = ValDec ([vb_x], nil)

val strexp_AA = BaseStr (SeqDec [dec_x])

val strb_AA = Strb {name = "AA",
		    def = strexp_AA,
		    constraint = NoSig}

val dec_AA = StrDec [strb_AA]

val strb_A = Strb {name = "A",
		   def = BaseStr (SeqDec [dec_AA]),
		   constraint = NoSig}

val dec_A = StrDec [strb_A]
		   
(* structure B *)

val dec_openA = OpenDec [["A"]]

val vb_y = Vb {pat = VarPat ["y"], exp = VarExp ["AA", "x"], lazyp = false}

val dec_y = ValDec ([vb_y], nil)
		   
val strexp_B = BaseStr (SeqDec [dec_openA, dec_y])

val strb_B = Strb {name = "B",
		   def = strexp_B,
		   constraint = NoSig}

val dec_B = StrDec [strb_B]

end (* top local *)
end (* structure Test *)
