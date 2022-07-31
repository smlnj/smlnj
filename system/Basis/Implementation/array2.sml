(* array2.sml
 *
 * COPYRIGHT (c) 1997 AT&T Research.
 *)

structure Array2 :> ARRAY2 =
  struct

    val ltu = InlineT.Int.ltu
    val unsafeUpdate = InlineT.PolyArray.update
    val unsafeSub = InlineT.PolyArray.sub

    structure A = Array
    structure AS = ArraySlice

    type 'a array = {
	data : 'a A.array, nrows : int, ncols : int
      }

    type 'a region = {
	base : 'a array,
	row : int, col : int,
	nrows : int option, ncols : int option
      }

    datatype traversal = RowMajor | ColMajor

    val mkArray = InlineT.PolyArray.array

  (* compute the index of an array element *)
    fun unsafeIndex ({nrows, ncols, ...} : 'a array, i, j) = (i*ncols + j)
    fun index (arr, i, j) =
	  if (ltu(i, #nrows arr) andalso ltu(j, #ncols arr))
	    then unsafeIndex (arr, i, j)
	    else raise General.Subscript

    fun chkSize (nrows, ncols) =
	  if (nrows < 0) orelse (ncols < 0)
	    then raise General.Size
	    else let
	      val n = nrows*ncols handle Overflow => raise General.Size
	      in
		if (Core.max_length < n) then raise General.Size else n
	      end

    fun array (nrows, ncols, v) = (case chkSize (nrows, ncols)
	   of 0 => {data = InlineT.PolyArray.newArray0(), nrows = 0, ncols = 0}
	    | n => {data = mkArray (n, v), nrows = nrows, ncols = ncols}
	  (* end case *))
    fun fromList rows = (case List.rev rows
	   of [] => {data = InlineT.PolyArray.newArray0(), nrows = 0, ncols = 0}
	    | (lastRow::rest) => let
		val nCols = List.length lastRow
		fun chk ([], nRows, l) = (nRows, l)
		  | chk (row::rest, nRows, l) = let
		      fun chkRow ([], n) = (
			    if (n <> nCols) then raise General.Size else ();
			    l)
			| chkRow (x::r, n) = x :: chkRow(r, n+1)
		      in
			chk (rest, nRows+1, chkRow(row, 0))
		      end
		val (nRows, data) = chk(rest, 1, lastRow)
		in
		  {data = Array.fromList data, nrows = nRows, ncols = nCols}
		end
	  (* end case *))
    fun tabulateRM (nrows, ncols, f) = (case chkSize (nrows, ncols)
	   of 0 => {data = InlineT.PolyArray.newArray0(), nrows = nrows, ncols = ncols}
	    | n => let
		val arr = mkArray (n, f(0, 0))
		fun lp1 (i, j, k) = if (i < nrows)
			then lp2 (i, 0, k)
			else ()
		and lp2 (i, j, k) = if (j < ncols)
			then (
			  unsafeUpdate(arr, k, f(i, j));
			  lp2 (i, j+1, k+1))
			else lp1 (i+1, 0, k)
		in
		  lp2 (0, 1, 1);  (* we've already done (0, 0) *)
		  {data = arr, nrows = nrows, ncols = ncols}
		end
	  (* end case *))
    fun tabulateCM (nrows, ncols, f) = (case chkSize (nrows, ncols)
	   of 0 => {data = InlineT.PolyArray.newArray0(), nrows = nrows, ncols = ncols}
	    | n => let
		val arr = mkArray (n, f(0, 0))
		val delta = n - 1
		fun lp1 (i, j, k) = if (j < ncols)
			then lp2 (0, j, k)
			else ()
		and lp2 (i, j, k) = if (i < nrows)
			then (
			  unsafeUpdate(arr, k, f(i, j));
			  lp2 (i+1, j, k+ncols))
			else lp1 (0, j+1, k-delta)
		in
		  lp2 (1, 0, ncols);  (* we've already done (0, 0) *)
		  {data = arr, nrows = nrows, ncols = ncols}
		end
	  (* end case *))
    fun tabulate RowMajor = tabulateRM
      | tabulate ColMajor = tabulateCM
    fun sub (a, i, j) = unsafeSub(#data a, index(a, i, j))
    fun update (a, i, j, v) = unsafeUpdate(#data a, index(a, i, j), v)
    fun dimensions {data, nrows, ncols} = (nrows, ncols)
    fun nCols (arr : 'a array) = #ncols arr
    fun nRows (arr : 'a array) = #nrows arr
    fun row ({data, nrows, ncols}, i) = let
	  val stop = i*ncols
	  fun mkVec (j, l) =
		if (j < stop)
		  then Vector.fromList l
		  else mkVec(j-1, A.sub(data, j)::l)
	  in
	    if ltu(nrows, i)
	      then raise General.Subscript
	      else mkVec (stop+ncols-1, [])
	  end
    fun column ({data, nrows, ncols}, j) = let
	  fun mkVec (i, l) =
		if (i < 0)
		  then Vector.fromList l
		  else mkVec(i-ncols, A.sub(data, i)::l)
	  in
	    if ltu(ncols, j)
	      then raise General.Subscript
	      else mkVec ((A.length data - ncols) + j, [])
	  end

    datatype index = DONE | INDX of {i:int, r:int, c:int}

    fun chkRegion {base={data, nrows, ncols}, row, col, nrows=nr, ncols=nc} = let
	  fun chk (start, n, NONE) =
		if ((start < 0) orelse (n < start))
		  then raise General.Subscript
		  else n-start
	    | chk (start, n, SOME len) =
		if ((start < 0) orelse (len < 0) orelse (n < start+len))
		  then raise General.Subscript
		  else len
	  val nr = chk (row, nrows, nr)
	  val nc = chk (col, ncols, nc)
	  in
	    {data = data, i = (row*ncols + col), r=row, c=col, nr=nr, nc=nc}
	  end

    fun copy {src : 'a region, dst: 'a array, dst_row, dst_col} =
	let val _ = chkRegion src
	    val { base, row = srow, col = scol,
		  nrows = snrows, ncols = sncols } = src
	    val { data = bdata, ncols = bncols, nrows = bnrows } = base
	    val { data = ddata, ncols = dncols, nrows = dnrows } = dst
	    val src_nrows = getOpt (snrows, bnrows - srow)
	    val src_ncols = getOpt (sncols, bncols - scol)
	    fun dn (i, d, s) =
		if i > 0 then
		    ((* we might be better off doing this directly
		      * instead of calling the ArraySlice module: *)
		     AS.copy { src = AS.slice (bdata, s, SOME src_ncols),
			       dst = ddata, di = d };
		     dn (i-1, d + dncols, s + bncols))
		else ()
	    fun up (i, d, s) =
		if i > 0 then
		    (AS.copy { src = AS.slice (bdata, s, SOME src_ncols),
			       dst = ddata, di = d };
		     up (i-1, d - dncols, s - bncols))
		else ()
	in if src_nrows + dst_row > dnrows orelse src_ncols + dst_col > dncols
	   then raise General.Subscript
	   else if dst_row <= srow then
	       dn (src_nrows,
		   dst_row * dncols + dst_col,
		   srow * bncols + scol)
	   else up (src_nrows,
		    (dst_row + src_nrows - 1) * dncols + dst_col,
		    (srow + src_nrows - 1) * bncols + scol)
	end

  (* this function generates a stream of indeces for the given region in
   * row-major order.
   *)
    fun iterateRM arg = let
	  val {data, i, r, c=cStart, nr, nc} = chkRegion arg
	  val ii = ref i and ri = ref r and ci = ref cStart
	  val rEnd = r+nr and cEnd = cStart+nc
	  val rowDelta = #ncols(#base arg) - nc
	  fun mkIndx (r, c) = let val i = !ii
		in
		  ii := i+1;
		  INDX{i=i, c=c, r=r}
		end
	  fun iter () = let
		val r = !ri and c = !ci
		in
		  if (c < cEnd)
		    then (ci := c+1; mkIndx(r, c))
		  else if (r+1 < rEnd)
		    then (
		      ii := !ii + rowDelta;
		      ci := cStart;
		      ri := r+1;
		      iter())
		    else DONE
		end
	  in
	    (data, iter)
	  end

  (* this function generates a stream of indeces for the given region in
   * col-major order.
   *)
    fun iterateCM (arg as {base={ncols, nrows, ...}, ...}) = let
	  val {data, i, r=rStart, c, nr, nc} = chkRegion arg
	  val ii = ref i and ri = ref rStart and ci = ref c
	  val rEnd = rStart+nr and cEnd = c+nc
	  val delta = (nr * ncols) - 1
	  fun mkIndx (r, c) = let val i = !ii
		in
		  ii := i+ncols;
		  INDX{i=i, c=c, r=r}
		end
	  fun iter () = let
		val r = !ri and c = !ci
		in
		  if (r < rEnd)
		    then (ri := r+1; mkIndx(r, c))
		  else if (c+1 < cEnd)
		    then (
		      ii := !ii - delta;
		      ri := rStart;
		      ci := c+1;
		      iter())
		    else DONE
		end
	  in
	    (data, iter)
	  end

    fun appi order f region = let
	  val (data, iter) = (case order
		 of RowMajor => iterateRM region
		  | ColMajor => iterateCM region
		(* end case *))
	  fun app () = (case iter()
		 of DONE => ()
		  | INDX{i, r, c} => (f(r, c, unsafeSub(data, i)); app())
		(* end case *))
	  in
	    app ()
	  end

    fun appRM f {data, ncols, nrows} = A.app f data
    fun appCM f {data, ncols, nrows} = let
	  val delta = A.length data - 1
	  fun appf (i, k) = if (i < nrows)
		then (f(unsafeSub(data, k)); appf(i+1, k+ncols))
		else let
		  val k = k-delta
		  in
		    if (k < ncols) then appf (0, k) else ()
		  end
	  in
	    appf (0, 0)
	  end
    fun app RowMajor = appRM
      | app ColMajor = appCM

    fun modifyi order f region = let
	  val (data, iter) = (case order
		 of RowMajor => iterateRM region
		  | ColMajor => iterateCM region
		(* end case *))
	  fun modify () = (case iter()
		 of DONE => ()
		  | INDX{i, r, c} => (
		      unsafeUpdate (data, i, f(r, c, unsafeSub(data, i)));
		      modify())
		(* end case *))
	  in
	    modify ()
	  end

    fun modifyRM f {data, ncols, nrows} = A.modify f data
    fun modifyCM f {data, ncols, nrows} = let
	  val delta = A.length data - 1
	  fun modf (i, k) = if (i < nrows)
		then (unsafeUpdate(data, k, f(unsafeSub(data, k))); modf(i+1, k+ncols))
		else let
		  val k = k-delta
		  in
		    if (k < ncols) then modf (0, k) else ()
		  end
	  in
	    modf (0, 0)
	  end
    fun modify RowMajor = modifyRM
      | modify ColMajor = modifyCM

    fun foldi order f init region = let
	  val (data, iter) = (case order
		 of RowMajor => iterateRM region
		  | ColMajor => iterateCM region
		(* end case *))
	  fun fold accum = (case iter()
		 of DONE => accum
		  | INDX{i, r, c} => fold(f(r, c, unsafeSub(data, i), accum))
		(* end case *))
	  in
	    fold init
	  end

    fun foldRM f init {data, ncols, nrows} = A.foldl f init data
    fun foldCM f init {data, ncols, nrows} = let
	  val delta = A.length data - 1
	  fun foldf (i, k, accum) = if (i < nrows)
		then foldf (i+1, k+ncols, f(unsafeSub(data, k), accum))
		else let
		  val k = k-delta
		  in
		    if (k < ncols) then foldf (0, k, accum) else accum
		  end
	  in
	    foldf (0, 0, init)
	  end
    fun fold RowMajor = foldRM
      | fold ColMajor = foldCM

  end
