(* Basics/util/list3.sml *)

(* utility list operators for 3 lists, like ListPair for 2 lists *)

structure List3 =
struct

  fun zip3 (x::xs,y::ys,z::zs) = (x,y,z) :: zip3(xs,ys,zs)
    | zip3 (nil,_,_) = nil
    | zip3 _ = raise Fail "zip3"

  fun zip3Eq (x::xs,y::ys,z::zs) = (x,y,z) :: zip3(xs,ys,zs)
    | zip3Eq (nil,nil,nil) = nil
    | zip3Eq _ = raise ListPair.UnequalLengths

end (* structure List3 *)
