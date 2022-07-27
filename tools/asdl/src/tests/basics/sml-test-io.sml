(* sml-test-io.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure TestIO =
  struct

    local
      open TestSpec
      structure Pkl = TestSpecFilePickle
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
    in
  (* tree *)
    fun chkTree () = let
	  val chk = check "tree" (U.tree_toString, U.tree_same, Pkl.write_tree, Pkl.read_tree)
	  in
	    chk EMPTY;
	    chk (NODE{value ="2", left=NODE{value ="1", left = EMPTY, right = EMPTY}, right=EMPTY});
	    chk (NODE{value ="1", left=EMPTY, right=NODE{value ="2", left = EMPTY, right = EMPTY}});
	    chk (NODE{
		value ="2",
		left=NODE{value ="1", left = EMPTY, right = EMPTY},
		right=NODE{value ="3", left = EMPTY, right = EMPTY}})
	  end
  (* coord *)
    fun chkCoord () = let
	  val chk = check "coord" (U.coord_toString, U.coord_same, Pkl.write_coord, Pkl.read_coord)
	  in
	    chk {x = 12, y = 13};
	    chk {x = ~12, y = 13}
	  end
  (* pos *)
    fun chkPos () = let
	  val chk = check "pos" (U.pos_toString, U.pos_same, Pkl.write_pos, Pkl.read_pos)
	  in
	    chk (12, 13);
	    chk (~12, 42)
	  end
  (* nat *)
    fun chkNat () = let
	  val chk = check "nat" (U.nat_toString, U.nat_same, Pkl.write_nat, Pkl.read_nat)
	  in
	    chk ZERO;
	    chk (SUCC ZERO);
	    chk (SUCC(SUCC(SUCC ZERO)))
	  end
  (* value *)
    fun chkValue () = let
	  val chk = check "value" (U.value_toString, U.value_same, Pkl.write_value, Pkl.read_value)
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
    fun chkColor () = let
	  val chk = check "color" (U.color_toString, U.color_same, Pkl.write_color, Pkl.read_color)
	  in
	    chk RED;
	    chk GREEN;
	    chk BLUE
	  end
  (* wrap_bool *)
    fun chkWrapBool () = let
	  val chk = check "wrap_bool" (U.wrap_bool_toString, U.wrap_bool_same, Pkl.write_wrap_bool, Pkl.read_wrap_bool)
	  in
	    chk (WRAP true);
	    chk (WRAP false)
	  end
  (* unit *)
    fun chkUnit () = let
	  val chk = check "unit" (U.unit_toString, U.unit_same, Pkl.write_unit, Pkl.read_unit)
	  in
	    chk UNIT
	  end
    end (* local *)

    fun chkAll () = (
	  chkUnit();
	  chkWrapBool();
	  chkColor();
	  chkValue();
	  chkNat();
	  chkPos();
	  chkCoord();
	  chkTree())

  end
