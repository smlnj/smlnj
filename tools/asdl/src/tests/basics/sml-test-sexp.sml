(* sml-test-sexp.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure TestSExp : sig

    val chk : unit -> unit

  end = struct

    local
      open TestSpec
      structure Pkl = TestSpecSExpPickle
      val toFile = ASDLSExpPickle.toFile Pkl.write_everything
    in
      fun chk () = let
	    val _ = toFile("everything.sexp", (
		    UNIT,
		    WRAP true,
		    GREEN,
		    STRING "hello world",
		    SUCC(SUCC(ZERO)),
		    (17, 42),
		    {x = ~17, y = ~42},
		    NODE{
		      value ="2",
		      left=NODE{value ="1", left = EMPTY, right = EMPTY},
		      right=NODE{value ="3", left = EMPTY, right = EMPTY}}
		  ))
	    in
	      ()
	    end
    end (* local *)

  end


