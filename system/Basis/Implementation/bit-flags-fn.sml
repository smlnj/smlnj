(* bit-flags-fn.sml
 *
 * COPYRIGHT (c) 2003 The Fellowship of SML/NJ
 *
 * Signature for bit flags.
 *
 *)
functor BitFlagsFn () :> BIT_FLAGS = struct

    structure SW = SysWordImp

    type flags = SW.word

    infix ++ &
    val op ++ = SW.orb
    val op & = SW.andb
    val neg = SW.notb

    fun toWord x = x
    fun fromWord x = x

    val all : flags = ~ 0w1
    val flags = foldl op ++ 0w0
    val intersect = foldl op & all
    fun clear (m, x) = x & (neg m)
    fun allSet (a, b) = (a ++ b) = b
    fun anySet (a, b) = (a & b) <> 0w0
end
