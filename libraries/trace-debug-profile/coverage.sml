(* coverage.sml
 *
 *   Using the generic trace/debug/profile framework for test coverage.
 *
 * Copyright (c) 2004 by The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)
structure Coverage : sig

    type kind

    val functions:      kind
    val tail_calls:     kind
    val non_tail_calls: kind

    val not_covered : kind list -> unit
    val hot_spots : kind list -> int -> unit

    val install : unit -> unit
end = struct

    structure M = IntRedBlackMap
    structure F = FormatComb

    structure TDP = SMLofNJ.Internals.TDP

    type kind = int
    val functions = TDP.idk_entry_point
    val tail_calls = TDP.idk_tail_call
    val non_tail_calls = TDP.idk_non_tail_call

    type record = { kind : int, descr: string }

    val records = ref (M.empty : record M.map)

    val counters = ref (Array.fromList [0])

    fun count idx = Array.sub (!counters, idx) handle General.Subscript => 0

    fun bump (module, id) =
	let val idx = module + id
	    val a = !counters
	in
	    Array.update (a, idx, Array.sub (a, idx) + 1)
	    handle General.Subscript =>
		   let val olen = Array.length a
		       val nlen = Int.min (idx + 1, olen + olen)
		       fun cp i = if i < olen then Array.sub (a, i)
				  else if i = idx then 1
				  else 0
		   in
		       counters := Array.tabulate (nlen, cp)
		   end
	end

    val enter = bump
    fun push mi = (bump mi; fn () => ())
    val nopush = bump

    fun register (module, kind, id, s) =
	let val idx = module + id
	    val r = { kind = kind, descr = s }
	in
	    records := M.insert (!records, idx, r)
	end

    fun save () () = ()

    val name = "coverage"

    fun install () =
	let val plugin = { name = name, save = save,
			   push = push, nopush = nopush,
			   enter = enter, register = register }
	    fun addto r x = r := x :: !r
	in
	    addto TDP.active_plugins plugin
	end

    fun not_covered kinds =
	let fun zerocnt (idx, r: record) =
		count idx = 0 andalso List.exists (fn k => k = #kind r) kinds
	    val zrecords = M.filteri zerocnt (!records)
	    fun tell { descr, kind } =
		Control.Print.say (descr ^ "\n")
	in
	    M.app tell zrecords
	end

    fun hot_spots kinds n =
	let fun getcount (idx, r: record) =
		if List.exists (fn k => k = #kind r) kinds then
		    SOME (#descr r, count idx)
		else NONE
	    val countmap = M.mapPartiali getcount (!records)
	    val countlist = M.listItems countmap
	    fun lt ((_, c), (_, c')) = c < c'
	    val sortedcountlist = ListMergeSort.sort lt countlist
	    fun loop ([], _) = ()
	      | loop (_, 0) = ()
	      | loop ((descr, count) :: rest, n) =
		  (Control.Print.say (F.format (F.padl 3 F.int o F.sp 1 o F.string o F.nl) count descr);
		   loop (rest, n - 1))
	in
	    loop (sortedcountlist, n)
	end
end
