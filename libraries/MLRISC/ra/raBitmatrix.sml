(*
 * Bit Matrix routines
 *)
structure RaBitmatrix : RA_BITMATRIX = struct
   structure W = Word
   structure A = Array
   structure UA = Unsafe.Array

   datatype bitMatrix = 
       BM of {table:hashTable, 
	      elems:int ref,
	      edges:int}
   and hashTable = 
       SMALL of word list Array.array ref * word
     | LARGE of bucket Array.array ref * word
  (* | BITMATRIX of Word8Array.array *)

   and bucket = NIL | B of int * int * bucket 

   exception Nodes
   fun hashFun(i, j, shift, size) = 
   let val i    = W.fromInt i
       val j    = W.fromInt j
       val h    = W.+(W.<<(i, shift), W.+(i, j))
       val mask = W.-(W.fromInt size, 0w1)
   in  W.toIntX(W.andb(h, mask)) end

   val empty = BM{table=SMALL(ref(A.array(2, [])), 0w0), elems=ref 0, edges=0}

   (*
   val indices = A.array(1024,0)

   fun init(i,j) =
       if i < 1024 then
	  (A.update(indices, i, j); init(i+1, i+j+1))
       else ()

   val _ = init(0, 0)
    *)
   fun size (BM{elems, ...}) = !elems

   fun edges(BM{table=SMALL(ref table, _), ...}) = A.length table
     | edges(BM{table=LARGE(ref table, _), ...}) = A.length table
   (*| edges(BM{table=BITMATRIX _, edges, ...}) = edges *)

   fun member(BM{table=SMALL(table, shift), ...}) =
       (fn (i, j) => 
	let val (i,j) = if i < j then (i, j) else (j, i)
	    val k = W.+(W.<<(W.fromInt i, 0w15), W.fromInt j)
	    fun find [] = false
	      | find(k'::b) = k = k' orelse find b
	    val tab = !table
	in  find(UA.sub(tab, hashFun(i, j, shift, A.length tab))) end
       )
     | member(BM{table=LARGE(table, shift), ...}) =
       (fn (i, j) => 
	let val (i,j) = if i < j then (i, j) else (j, i)
	    fun find NIL = false
	      | find(B(i',j',b)) = i = i' andalso j = j' orelse find b
	    val tab = !table
	in  find(UA.sub(tab, hashFun(i, j, shift, A.length tab))) end
       )
     (*
     | member(BM{table=BITMATRIX table, ...}) =
       (fn (i, j) => 
	let val (i,j) = if i > j then (i, j) else (j, i)
	    val bit   = W.fromInt(UA.sub(indices, i) + j)
	    val index = W.toIntX(W.>>(bit, 0w3))
	    val mask  = W.<<(0w1, W.andb(bit, 0w7))
	in  W.andb(W.fromInt(W8.toInt(UW8A.sub(table, index))), mask) <> 0w0 
	end
       )
      *)

   fun add (BM{table=SMALL(table, shift), elems, ...}) =
       let fun insert(i, j) =
	   let val (i,j) = if i < j then (i, j) else (j, i)
	       val tab = !table
	       val len = A.length tab
	   in  if !elems < len then
	       let val index = hashFun(i, j, shift, len)
		   val k = W.+(W.<<(W.fromInt i, 0w15), W.fromInt j)
		   fun find [] = false
		     | find(k'::b) = k = k' orelse find b
		   val b = UA.sub(tab, index)
	       in  if find b then false
		   else (UA.update(tab, index, k::b); 
			 elems := !elems + 1; true)
	       end
	       else (* grow table *)
	       let val oldTable = tab
		   val oldSize  = A.length oldTable
		   val newSize  = oldSize + oldSize
		   val newTable = A.array(newSize,[])
		   fun enter n =
		   if n < oldSize then
		   let fun loop([],a,b) = 
			     (UA.update(newTable, n, a);
			      UA.update(newTable, n + oldSize, b);
			      enter(n+1))
			 | loop(k::l,a,b) =
			   let val i = W.toIntX(W.>>(k, 0w15))  
			       val j = W.toIntX(W.-(k,W.<<(W.fromInt i, 0w15)))
			   in  if hashFun(i, j, shift, newSize) = n 
			       then loop(l, k::a, b)
			       else loop(l, a, k::b)
			   end
		   in  loop(UA.sub(oldTable, n), [], []) end
		   else ()
	       in  table := newTable;
		   enter 0; 
		   insert(i, j)
	       end 
	   end
       in  insert
       end
     | add (BM{table=LARGE(table, shift), elems, ...}) =
       let fun insert(i, j) =
	   let val (i,j) = if i < j then (i, j) else (j, i)
	       val tab = !table
	       val len = A.length tab
	   in  if !elems < len then
	       let val index = hashFun(i, j, shift, len)
		   fun find NIL = false
		     | find(B(i',j',b)) = i = i' andalso j = j' orelse find b
		   val b = UA.sub(tab, index)
	       in  if find b then false
		   else (UA.update(tab, index, B(i,j,b)); 
			 elems := !elems + 1; true)
	       end
	       else (* grow table *)
	       let val oldTable = tab
		   val oldSize  = A.length oldTable
		   val newSize  = oldSize + oldSize
		   val newTable = A.array(newSize,NIL)
		   fun enter n =
		   if n < oldSize then
		   let fun loop(NIL,a,b) = 
			     (UA.update(newTable, n, a);
			      UA.update(newTable, n + oldSize, b);
			      enter(n+1))
			 | loop(B(i,j,l),a,b) =
			      if hashFun(i, j, shift, newSize) = n 
			      then loop(l, B(i,j,a), b)
			      else loop(l, a, B(i,j,b))
		   in  loop(UA.sub(oldTable, n), NIL, NIL) end
		   else ()
	       in  table := newTable;
		   enter 0; 
		   insert(i, j)
	       end 
	   end
       in  insert
       end
     (*
     | add(BM{table=BITMATRIX table, ...}) =
       (fn (i, j) =>
	let val (i,j) = if i > j then (i, j) else (j, i)
	    val bit   = W.fromInt(UA.sub(indices, i) + j)
	    val index = W.toIntX(W.>>(bit, 0w3))
	    val mask  = W.<<(0w1, W.andb(bit, 0w7))
	    val value = W.fromInt(W8.toInt(UW8A.sub(table, index)))
	in  if W.andb(value, mask) <> 0w0 then false
	    else (UW8A.update(table, index, 
		    W8.fromInt(W.toIntX(W.orb(value, mask)))); true) 
	end
       )
      *)

   fun delete (BM{table=SMALL(table, shift), elems, ...}) =
       (fn (i,j) =>
	let val k = W.+(W.<<(W.fromInt i, 0w15), W.fromInt j)
	    fun find [] = []
	      | find(k'::b) =
		if k = k' then (elems := !elems - 1; b) else k'::find b
	    val tab = !table
	    val index = hashFun(i, j, shift, A.length tab)
	    val n = !elems
	in  UA.update(tab, index, find(UA.sub(tab, index)));
	    !elems <> n
	end
       )
     | delete (BM{table=LARGE(table, shift), elems, ...}) =
       (fn (i,j) =>
	let fun find NIL = NIL
	      | find(B(i', j', b)) =
		if i = i' andalso j = j' then (elems := !elems - 1; b)
		else B(i', j', find b)
	    val tab = !table
	    val index = hashFun(i, j, shift, A.length tab)
	    val n = !elems
	in  UA.update(tab, index, find(UA.sub(tab, index)));
	    !elems <> n
	end
       )
   (*
     | delete(BM{table=BITMATRIX table, ...}) =
       (fn (i, j) =>
	let val (i,j) = if i > j then (i, j) else (j, i)
	    val bit   = W.fromInt(UA.sub(indices, i) + j)
	    val index = W.toIntX(W.>>(bit, 0w3))
	    val mask  = W.-(W.<<(0w1, W.andb(bit, 0w7)), 0w1)
	    val value = W.fromInt(W8.toInt(UW8A.sub(table, index)))
	in  if W.andb(value, mask) = 0w0 then false
	    else (UW8A.update(table, index, 
			  W8.fromInt(W.toIntX(W.andb(value,W.notb mask)))); 
		  true) 
	end
       )
    *)
end
