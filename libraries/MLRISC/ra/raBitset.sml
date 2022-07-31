(* raBitset.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * Imperative bitsets.
 *
 * This has been written specially for the register allocator.
 * The computation of n(n+1)/2 very quickly overflows in practice.
 *
 *)

(** This has been written specially for the register allocator.
 ** We use a hash table representation, because it performs better
 ** than a linear representation except for small numbers of live 
 ** ranges.
 **)

signature BITMATRIX = sig
    type bitMatrix
    val new         : int -> bitMatrix
    val add         : bitMatrix -> (int * int) -> bool
    val member      : bitMatrix -> (int * int) -> bool
    val delete      : bitMatrix -> (int * int) -> bool
(*  val clear 	    : bitMatrix * int -> unit
*)
end


structure TriangularBitMatrix :> BITMATRIX = 
struct

  datatype bucket = NIL | B of (int * int * bucket)

  datatype bitMatrix = 
      INTPAIRMAP of {table : bucket Array.array ref, 
		     elems : int ref, 
		     size : word ref, 
		     shift : word}
  val itow = Word.fromInt
  val wtoi = Word.toInt
  fun roundsize size = let
      fun f(x, shift) = 
	if x >= size then (x, Word.>>(shift, 0w1)) 
	else f(x*2, Word.+(shift,0w1))
    in f(64, 0w6)
    end

  fun new size = let 
      val (tblSize, shift) = roundsize size   
      val tbl = Array.array(tblSize,NIL)
    in 					(* note: size is offset by 1 *)
       INTPAIRMAP{table    = ref tbl,
		  elems    = ref 0, 
		  size     = ref(itow(tblSize-1)),
		  shift	   = shift}
    end

  fun moduloSize(i, j, shift, sz) = 
    Word.toIntX
      (Word.andb
         (Word.+(Word.<<(itow i, shift), itow j),
	  sz))
			 
  fun member(INTPAIRMAP{table,elems,size,shift,...}) (i,j) = let
	fun find NIL = false
	  | find(B(i',j',b)) = (i=i' andalso j=j') orelse find b
      in find(Array.sub(!table, moduloSize(i, j, shift, !size)))
      end

  fun add (t as INTPAIRMAP{table,elems,size,shift,...}) (v as (i,j)) = let
	val ref tbl = table
	val ref sz = size
	val isz = wtoi sz
      in
	if !elems <> isz then let
	    val indx = moduloSize(i, j, shift, sz)
	    fun find NIL = false
	      | find(B(i',j',r)) = (i=i' andalso j=j') orelse find r
	    val b = Array.sub(tbl,indx)
	  in 
	     if find b then false
	     else (Unsafe.Array.update(tbl,indx,B(i,j,b)); 
		   elems := !elems + 1;
		   true)
	  end
	else let 
	     val newsize=isz+isz+2
	     val new = Array.array(newsize,NIL)
	     val newsize1 = itow(newsize-1)
	     fun redo n = let
	       fun add'(a,b,B(i,j,r)) = 
		   if moduloSize(i, j, shift, newsize1) = n then
		     add'(B(i,j,a),b,r)
		   else add'(a,B(i,j,b),r)
		 | add'(a,b,NIL) = 
		     (Array.update(new,n,a); 
		      Array.update(new,n+isz+1,b);
		      redo(n+1))
	     in add'(NIL, NIL, Array.sub(tbl,n))
	     end
	  in 
	     table:=new;
	     size:=itow(newsize-1);
	     redo 0 handle _ => ();
	     add t v
	  end
      end

  fun delete(INTPAIRMAP{table=ref table,elems,size,shift,...}) (i,j) = let
    fun find NIL = NIL
      | find(B(i',j',b)) =
	  if i=i' andalso j=j' then (elems := !elems-1; b) else B(i',j',find b)
    val indx = moduloSize(i, j, shift, !size)
    val n = !elems
  in Unsafe.Array.update(table, indx, find(Array.sub(table,indx)));
     !elems <> n (* changed? *)
  end

end

