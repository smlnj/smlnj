(* test.sml
 *
 * COPYRIGHT (c) 2014 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Test =
  struct

    structure XT = GenericXMLTree
    structure Parse = XMLParserFn(XT)

    fun parse file = (
	  print(concat ["parsing \"", file, "\" ... "]);
	  (ignore (Parse.parseFile file); print " ok\n")
	    handle Parse.ParseError msg => print(concat["failed: ", msg, "\n"]))

    fun t_20130930 () = parse "test-files/20130930.xml"
    fun t_Info () = parse "test-files/Info.plist"
    fun t_egl () = parse "test-files/egl.xml"
    fun t_extreme3d () = parse "test-files/extreme-3d-pro.xml"
    fun t_gl () = parse "test-files/gl.xml"
    fun t_glx () = parse "test-files/glx.xml"
    fun t_rust () = parse "test-files/rust.xml"
    fun t_wgl () = parse "test-files/wgl.xml"

  end
