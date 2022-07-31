(* Copyright (c) 1998 by Lucent Technologies *)

(* a functor for creating new categories of unique ids *)

functor UidFn (val initial: int
	       val prefix: string) :> UID =
struct

  type uid = int

  val initial = initial

  val counter = ref initial

  fun new () = let val n = !counter
                in counter := n + 1;
		   n
	       end

  fun reset n = counter := n

  fun toString x = prefix ^ (Int.toString x)

  val toWord = Word.fromInt

  fun equal (uid:uid, uid') = (uid = uid')

  val compare = Int.compare

end

