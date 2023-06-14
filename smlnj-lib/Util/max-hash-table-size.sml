(* max-hash-table-size.sml
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Common code for computing an upper-limit on the size of hash tables
 * (hash-table-rep.sml) and hash sets (hash-set-fn.sml).
 *)

structure MaxHashTableSize : sig

    val maxSize : int

  end = struct

    structure W = Word

    (* return largest k such that 2^k <= n *)
    fun log2 (n : int) = let
          fun lp (n, k) = if (n <= 0) then k-1 else lp(n div 2, k+1)
          in
            lp(n, 0)
          end

    (* return 2^k *)
    fun pow2 k = let
          fun lp (k, n) = if (k <= 0) then n else lp(k-1, n+n)
          in
            lp (k, 1)
          end

    (* pick the largest power of 2 that is <= than the maximum array size *)
    val maxSize = pow2 (log2 Array.maxLen)

  end
