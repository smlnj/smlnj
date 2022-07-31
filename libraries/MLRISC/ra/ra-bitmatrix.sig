
signature RA_BITMATRIX = sig

  datatype bucket = NIL | B of int * int * bucket 
  datatype hashTable = 
      SMALL of word list Array.array ref * word
    | LARGE of bucket Array.array ref * word
 (* | BITMATRIX of Word8Array.array *)

  datatype bitMatrix = 
     BM of {table:hashTable, 
            elems:int ref,
            edges:int}

  val empty : bitMatrix
  val edges : bitMatrix -> int
  val size : bitMatrix -> int
  val add : bitMatrix -> (int * int) -> bool
  val member : bitMatrix -> (int * int) -> bool
end
