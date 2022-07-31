(* Copyright 1996 by AT&T Bell Laboratories *)
(* entpath.sml *)

signature ENT_PATH = sig

  type entVar = Stamps.stamp
  type entPath = entVar list
  type rEntPath

  val epnil : entPath
  val repnil : rEntPath
  val repcons : entVar * rEntPath -> rEntPath

  val ep2rep : entPath * rEntPath -> rEntPath
  val rep2ep : rEntPath -> entPath

  val eqEntVar : entVar * entVar -> bool
  val eqEntPath : entPath * entPath -> bool

  val cmpEntVar : entVar * entVar -> order
  val cmpEntPath : entPath * entPath -> order

  val nullEntPath : entPath -> bool
  val entVarToString : entVar -> string
  val entPathToString : entPath -> string

  val bogusEntVar : entVar

  structure EvDict : ORD_MAP where type Key.ord_key = entVar

end  (* signature ENT_PATH *)


structure EntPath :> ENT_PATH =
struct

local
  structure ST = Stamps
in

type entVar = ST.stamp

type entPath = entVar list
(* entPath has entVars in direct order, outer first *)

type rEntPath = entVar list		(* reversed order; abstract *)

val epnil = []
val repnil = []
val repcons = op ::

val ep2rep = List.revAppend
val rep2ep = rev

val eqEntVar = ST.eq

(* eqEntPath: elementwise equality of entPaths *)
val eqEntPath = ListPair.allEq eqEntVar

val cmpEntVar = ST.compare

(* cmpEntPath: entPath * entPath -> order
 * lexicographic comparison of two entPaths *)
fun cmpEntPath (ep1, ep2) = 
  let fun f(a::ar, b::br) =
            (case ST.compare(a,b) of EQUAL => f(ar,br) | z => z)
        | f(a::ar, nil) = GREATER
        | f(nil, b::br) = LESS
        | f(nil,nil) = EQUAL
   in f(ep1,ep2)
  end

structure EvDict =
  RedBlackMapFn(struct type ord_key = entVar 
                       val compare = cmpEntVar
                end)

fun nullEntPath(ep: entPath) = List.null ep

fun entVarToString (v: entVar) = ST.toShortString v

fun entPathToString ([]: entPath) = "[]"
  | entPathToString (x::xs) =
      let val rest = foldr (fn (y,l) => ","::(ST.toShortString y)::l) ["]"] xs
       in String.concat("[" :: (ST.toShortString x) :: rest)
      end

val bogusEntVar = ST.special "bogusEntVar"

end (* local *)
end (* structure EntPath *)
