(* sml-test-mix.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Tests interaction between memory and file pickling.
 *)

structure TestMix =
  struct

    local
      open TestSpec
      structure Pkl = TestSpecMemoryPickle
      structure PIO = TestSpecFilePickle
      structure U = Util
    (* pickle/unpickle identity *)
      fun ident (pickle, unpickle) x = let
	    val file = OS.FileSys.tmpName()
	    val outS = BinIO.openOut file
	    val _ = pickle (outS, x) handle exn => (BinIO.closeOut outS; raise exn)
	    val _ = BinIO.closeOut outS
	    val inS = BinIO.openIn file
	    val y = unpickle inS handle exn => (BinIO.closeIn inS; raise exn)
	    val _ = BinIO.closeIn inS
	    in
	      OS.FileSys.remove file;
	      y
	    end
    (* pickle by first converting to bytes and then writing the bytes *)
      fun wrapPkl pick (outS, x) = BinIO.output(outS, ASDLMemoryPickle.toVector pick x)
      fun wrapUnkpkl unpick inS = ASDLMemoryPickle.fromVector unpick (BinIO.inputAll inS)
    (* check that the pickle/unpickle cycle preserves values *)
      fun check name (toStr, same, pick, unpick) x = let
	    val _ = print(concat["check ", name, ": unpickle(pickle ", toStr x, ")"])
	    val y = ident (pick, unpick) x
	    in
	      if same(x, y)
		then print " ok\n"
		else print(concat[" fail (", toStr y, ")\n"])
	    end
	      handle exn => print(concat[" fail(", exnMessage exn, ")\n"])
      fun checkFM name (toStr, same, wr, unpick) = check ("FM "^name) (toStr, same, wr, wrapUnkpkl unpick)
      fun checkMF name (toStr, same, pick, rd) = check ("MF "^name) (toStr, same, wrapPkl pick, rd)
    in
  (* tree: file -> memory *)
    fun chkTreeFM () = let
	  fun chk tr = (
		checkFM "tree" (U.tree_toString, U.tree_same, PIO.write_tree, Pkl.read_tree) tr;
		checkMF "tree" (U.tree_toString, U.tree_same, Pkl.write_tree, PIO.read_tree) tr)
	  in
	    chk EMPTY;
	    chk (NODE{value ="2", left=NODE{value ="1", left = EMPTY, right = EMPTY}, right=EMPTY});
	    chk (NODE{value ="1", left=EMPTY, right=NODE{value ="2", left = EMPTY, right = EMPTY}});
	    chk (NODE{
		value ="2",
		left=NODE{value ="1", left = EMPTY, right = EMPTY},
		right=NODE{value ="3", left = EMPTY, right = EMPTY}})
	  end
  (* tree: memory -> file *)
  (* coord *)
    fun chkCoordFM () = let
	  fun chk tr = (
		checkFM "coord" (U.coord_toString, U.coord_same, PIO.write_coord, Pkl.read_coord) tr;
		checkMF "coord" (U.coord_toString, U.coord_same, Pkl.write_coord, PIO.read_coord) tr)
	  in
	    chk {x = 12, y = 13};
	    chk {x = ~12, y = 13}
	  end
  (* pos *)
    fun chkPosFM () = let
	  fun chk tr = (
		checkFM "pos" (U.pos_toString, U.pos_same, PIO.write_pos, Pkl.read_pos) tr;
		checkMF "pos" (U.pos_toString, U.pos_same, Pkl.write_pos, PIO.read_pos) tr)
	  in
	    chk (12, 13);
	    chk (~12, 42)
	  end
  (* nat *)
    fun chkNatFM () = let
	  fun chk tr = (
		checkFM "nat" (U.nat_toString, U.nat_same, PIO.write_nat, Pkl.read_nat) tr;
		checkMF "nat" (U.nat_toString, U.nat_same, Pkl.write_nat, PIO.read_nat) tr)
	  in
	    chk ZERO;
	    chk (SUCC ZERO);
	    chk (SUCC(SUCC(SUCC ZERO)))
	  end
  (* value *)
    fun chkValueFM () = let
	  fun chk tr = (
		checkFM "value" (U.value_toString, U.value_same, PIO.write_value, Pkl.read_value) tr;
		checkMF "value" (U.value_toString, U.value_same, Pkl.write_value, PIO.read_value) tr)
	  in
	    chk (BOOL false);
	    chk (BOOL true);
	    chk (INT ~1);
	    chk (INT 0);
	    chk (INT 1);
	    chk (INT 42);
	    chk (STRING "");
	    chk (STRING "a");
	    chk (STRING "abc\n")
	  end
  (* color *)
    fun chkColorFM () = let
	  fun chk tr = (
		checkFM "color" (U.color_toString, U.color_same, PIO.write_color, Pkl.read_color) tr;
		checkMF "color" (U.color_toString, U.color_same, Pkl.write_color, PIO.read_color) tr)
	  in
	    chk RED;
	    chk GREEN;
	    chk BLUE
	  end
  (* wrap_bool *)
    fun chkWrapBoolFM () = let
	  fun chk tr = (
		checkFM "wrap_bool"
		  (U.wrap_bool_toString, U.wrap_bool_same, PIO.write_wrap_bool, Pkl.read_wrap_bool) tr;
		checkMF "wrap_bool"
		  (U.wrap_bool_toString, U.wrap_bool_same, Pkl.write_wrap_bool, PIO.read_wrap_bool) tr)
	  in
	    chk (WRAP true);
	    chk (WRAP false)
	  end
  (* unit *)
    fun chkUnitFM () = let
	  fun chk tr = (
		checkFM "unit" (U.unit_toString, U.unit_same, PIO.write_unit, Pkl.read_unit) tr;
		checkMF "unit" (U.unit_toString, U.unit_same, Pkl.write_unit, PIO.read_unit) tr)
	  in
	    chk UNIT
	  end
    end (* local *)

    fun chkAll () = (
	  chkUnitFM();
	  chkWrapBoolFM();
	  chkColorFM();
	  chkValueFM();
	  chkNatFM();
	  chkPosFM();
	  chkCoordFM();
	  chkTreeFM())

  end
