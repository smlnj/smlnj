(* test.sml
 *
 * COPYRIGHT (c) 2022, The Fellowship of SML/NJ (https://www.smlnj.org)
 *)

(* [DBM: 20202.12.19] This is an naive (flawed) attempt to translate the test.sml file from
 *  smlnj-lib/PP/tests/test.sml to work with NewPrettyPrint. The test functions and test cases
 *  need to be redesigned for NewPrettyPrint. *)

local

  open NewPrettyPrint

  fun repeat c n = StringCvt.padLeft c n ""

  (* simple1 : string * int * int * (format list -> format) -> unit
     name : test name,
     w: int -- line width,
     n: int -- string length
     mkblock : format list -> format -- block making function *)
  fun simple1 (name: string, w: int, n: int, mkblock) () =
	printFormatLW w
          (vcat
	     (text name,   
	      mkblock [text (repeat #"x" n), text (repeat #"y" n), text (repeat #"z" n)]))


  (* simple2 : string * int * int * (format list -> format) * (format list -> format) -> unit *)
  fun simple2 (name: string, w: int, n: int, mkblock_outer, mkblock_inner) () =
        printFormatLW w
	  (vcat
	     (text name,
	      mkblock_outer  (* always broken because of hardIndent call *)
		[text (repeat #"v" n),
		 hardIndent 2 
		   (mkblock_inner [text (repeat #"w" n), text (repeat #"x" n), text (repeat #"y" n)]),
		 text (repeat #"z" n)]))

in

(* BOGUS! Replaced "openBox" with "cblock", but openBox does not correspond with cblock!
 *   openBox is a variant of openHOVBox (pblock) with a weird undent behavior relating to
 *   openXBox indent parameters. It is not at all clear whether we can replace openBox in
 *   a sensible manner. *)

val t01a = simple1 ("Test 01a [hblock]", 10, 2, hblock)
val t01b = simple1 ("Test 01b [hblock]", 10, 3, hblock)
val t02a = simple1 ("Test 02a [vblock]", 10, 2, vblock)
val t02b = simple1 ("Test 02b [vblock]", 10, 3, vblock)
val t03a = simple1 ("Test 03a [hvblock]", 10, 2, hvblock)
val t03b = simple1 ("Test 03b [hvblock]", 10, 4, hvblock)
val t04a = simple1 ("Test 04a [pblock]", 10, 2, pblock)
val t04b = simple1 ("Test 04b [pblock]", 10, 4, pblock)
val t05a = simple1 ("Test 05a [cblock]", 10, 2, cblock)
val t05b = simple1 ("Test 05b [cblock]", 10, 4, cblock)

val t11a = simple2 ("Test 11a [hblock/hblock]", 10, 2, hblock, hblock)
val t11b = simple2 ("Test 11b [hblock/hblock]", 10, 3, hblock, hblock)
val t11c = simple2 ("Test 11c [hblock/hblock]", 10, 4, hblock, hblock)
val t12a = simple2 ("Test 12a [hblock/vblock]", 10, 2, hblock, vblock)
val t12b = simple2 ("Test 12b [hblock/vblock]", 10, 3, hblock, vblock)
val t12c = simple2 ("Test 12c [hblock/vblock]", 10, 4, hblock, vblock)
val t13a = simple2 ("Test 13a [hblock/hvblock]", 10, 2, hblock, hvblock)
val t13b = simple2 ("Test 13b [hblock/hvblock]", 10, 3, hblock, hvblock)
val t13c = simple2 ("Test 13c [hblock/hvblock]", 10, 4, hblock, hvblock)
val t14a = simple2 ("Test 14a [hblock/pblock]", 10, 2, hblock, pblock)
val t14b = simple2 ("Test 14b [hblock/pblock]", 10, 3, hblock, pblock)
val t14c = simple2 ("Test 14c [hblock/pblock]", 10, 4, hblock, pblock)
val t15a = simple2 ("Test 15a [hblock/cblock]", 10, 2, hblock, cblock)
val t15b = simple2 ("Test 15b [hblock/cblock]", 10, 3, hblock, cblock)
val t15c = simple2 ("Test 15c [hblock/cblock]", 10, 4, hblock, cblock)
val t16a = simple2 ("Test 16a [vblock/hblock]", 10, 2, vblock, hblock)
val t16b = simple2 ("Test 16b [vblock/hblock]", 10, 3, vblock, hblock)
val t16c = simple2 ("Test 16c [vblock/hblock]", 10, 4, vblock, hblock)
val t17a = simple2 ("Test 17a [vblock/vblock]", 10, 2, vblock, vblock)
val t17b = simple2 ("Test 17b [vblock/vblock]", 10, 3, vblock, vblock)
val t17c = simple2 ("Test 17c [vblock/vblock]", 10, 4, vblock, vblock)
val t18a = simple2 ("Test 18a [vblock/hvblock]", 10, 2, vblock, hvblock)
val t18b = simple2 ("Test 18b [vblock/hvblock]", 10, 3, vblock, hvblock)
val t18c = simple2 ("Test 18c [vblock/hvblock]", 10, 4, vblock, hvblock)
val t19a = simple2 ("Test 19a [vblock/pblock]", 10, 2, vblock, pblock)
val t19b = simple2 ("Test 19b [vblock/pblock]", 10, 3, vblock, pblock)
val t19c = simple2 ("Test 19c [vblock/pblock]", 10, 4, vblock, pblock)
val t20a = simple2 ("Test 20a [vblock/cblock]", 10, 2, vblock, cblock)
val t20b = simple2 ("Test 20b [vblock/cblock]", 10, 3, vblock, cblock)
val t20c = simple2 ("Test 20c [vblock/cblock]", 10, 4, vblock, cblock)
val t21a = simple2 ("Test 21a [hvblock/hblock]", 10, 2, hvblock, hblock)
val t21b = simple2 ("Test 21b [hvblock/hblock]", 10, 3, hvblock, hblock)
val t21c = simple2 ("Test 21c [hvblock/hblock]", 10, 4, hvblock, hblock)
val t22a = simple2 ("Test 22a [hvblock/vblock]", 10, 2, hvblock, vblock)
val t22b = simple2 ("Test 22b [hvblock/vblock]", 10, 3, hvblock, vblock)
val t22c = simple2 ("Test 22c [hvblock/vblock]", 10, 4, hvblock, vblock)
val t23a = simple2 ("Test 23a [hvblock/hvblock]", 10, 2, hvblock, hvblock)
val t23b = simple2 ("Test 23b [hvblock/hvblock]", 10, 3, hvblock, hvblock)
val t23c = simple2 ("Test 23c [hvblock/hvblock]", 10, 4, hvblock, hvblock)
val t24a = simple2 ("Test 24a [hvblock/pblock]", 10, 2, hvblock, pblock)
val t24b = simple2 ("Test 24b [hvblock/pblock]", 10, 3, hvblock, pblock)
val t24c = simple2 ("Test 24c [hvblock/pblock]", 10, 4, hvblock, pblock)
val t25a = simple2 ("Test 25a [hvblock/cblock]", 10, 2, hvblock, cblock)
val t25b = simple2 ("Test 25b [hvblock/cblock]", 10, 3, hvblock, cblock)
val t25c = simple2 ("Test 25c [hvblock/cblock]", 10, 4, hvblock, cblock)
val t26a = simple2 ("Test 26a [pblock/hblock]", 10, 2, pblock, hblock)
val t26b = simple2 ("Test 26b [pblock/hblock]", 10, 3, pblock, hblock)
val t26c = simple2 ("Test 26c [pblock/hblock]", 10, 4, pblock, hblock)
val t27a = simple2 ("Test 27a [pblock/vblock]", 10, 2, pblock, vblock)
val t27b = simple2 ("Test 27b [pblock/vblock]", 10, 3, pblock, vblock)
val t27c = simple2 ("Test 27c [pblock/vblock]", 10, 4, pblock, vblock)
val t28a = simple2 ("Test 28a [pblock/hvblock]", 10, 2, pblock, hvblock)
val t28b = simple2 ("Test 28b [pblock/hvblock]", 10, 3, pblock, hvblock)
val t28c = simple2 ("Test 28c [pblock/hvblock]", 10, 4, pblock, hvblock)
val t29a = simple2 ("Test 29a [pblock/pblock]", 10, 2, pblock, pblock)
val t29b = simple2 ("Test 29b [pblock/pblock]", 10, 3, pblock, pblock)
val t29c = simple2 ("Test 29c [pblock/pblock]", 10, 4, pblock, pblock)
val t30a = simple2 ("Test 30a [pblock/cblock]", 10, 2, pblock, cblock)
val t30b = simple2 ("Test 30b [pblock/cblock]", 10, 3, pblock, cblock)
val t30c = simple2 ("Test 30c [pblock/cblock]", 10, 4, pblock, cblock)
val t31a = simple2 ("Test 31a [cblock/hblock]", 10, 2, cblock, hblock)
val t31b = simple2 ("Test 31b [cblock/hblock]", 10, 3, cblock, hblock)
val t31c = simple2 ("Test 31c [cblock/hblock]", 10, 4, cblock, hblock)
val t32a = simple2 ("Test 32a [cblock/vblock]", 10, 2, cblock, vblock)
val t32b = simple2 ("Test 32b [cblock/vblock]", 10, 3, cblock, vblock)
val t32c = simple2 ("Test 32c [cblock/vblock]", 10, 4, cblock, vblock)
val t33a = simple2 ("Test 33a [cblock/hvblock]", 10, 2, cblock, hvblock)
val t33b = simple2 ("Test 33b [cblock/hvblock]", 10, 3, cblock, hvblock)
val t33c = simple2 ("Test 33c [cblock/hvblock]", 10, 4, cblock, hvblock)
val t34a = simple2 ("Test 34a [cblock/pblock]", 10, 2, cblock, pblock)
val t34b = simple2 ("Test 34b [cblock/pblock]", 10, 3, cblock, pblock)
val t34c = simple2 ("Test 34c [cblock/pblock]", 10, 4, cblock, pblock)
val t35a = simple2 ("Test 35a [cblock/cblock]", 10, 2, cblock, cblock)
val t35b = simple2 ("Test 35b [cblock/cblock]", 10, 3, cblock, cblock)
val t35c = simple2 ("Test 35c [cblock/cblock]", 10, 4, cblock, cblock)
 
fun t40 () =
    printFormatLW 20 
      (vcat
        (text "Test 20 [C code]",
         pblock
	   [hblock [text "if", text "(x < y)", text "{"],
	    softIndent 4
	      (hvblock
	         [text "stmt1;",
		  hvblock
		    [hblock [text "if", text "(w < z)", text "{"],
		     softIndent 4
		       (hvblock [text "stmt2;" text "stmt3;", text "stmt4;"]),
		     text "}"]]),
	    text "stmt5;",
	    text "stmt6;",
	    text "}"]))

(* a test of vblock ??? *)
fun t50 () =
    printFormatLW 20 
     (vcat
	(text "Test 40 [vbox]",
	 let fun strings l = hblock (map text l)
	  in vblock 
	       [strings ["0:", "line", "1"],
		strings ["0:", "line", "2"],
		hardIndent 2
		  (vblock
		     [strings ["2:", "line", "3"],
		      strings ["2:", "line", "4"]]),
		hardIndent 2
		  (vblock
		     [strings ["2:", "line", "5"],
		      strings ["2:", "line", "6"]]),

		strings ["0:", "line", "7"],
		strings ["0:", "line", "8"],
		hardIndent 4
		  (vblock
		     [strings ["4:", "line", "9"]
		      strings ["4:", "line", "10"]]),
		strings ["0:", "line", "11"],
		strings ["0:", "line", "12"]]
	 end))

end (* local *)
