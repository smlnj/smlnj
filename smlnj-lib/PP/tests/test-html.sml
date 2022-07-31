(* test.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *)

CM.make(); (* to compile and load the PP library *)

structure TextToken =
  struct
    type token = string
    type style = HTMLDev.style
    fun string t = t
    fun style t = HTMLDev.styleTT
    fun size t = String.size t
  end;

structure PP = PPDebugFn(PPStreamFn(
    structure Token = TextToken
    structure Device = HTMLDev));

fun kw strm s = (
      PP.pushStyle(strm, HTMLDev.styleB);
      PP.string strm s;
      PP.popStyle strm)

fun withPP (name, wid) ppFn = let
      val saveStrm = !PP.debugStrm
      val _ = PP.debugStrm := TextIO.openAppend("out")
      val ppDev = HTMLDev.openDev{wid=wid, textWid=NONE}
      val ppStrm = PP.openStream ppDev
      in
	PP.pushStyle(ppStrm, HTMLDev.styleTT);
	ppFn ppStrm;
        PP.popStyle ppStrm;
	PP.closeStream ppStrm;
	PrHTML.prHTML {
	    putc = fn c => TextIO.output1 (TextIO.stdOut, c),
	    puts = fn s => TextIO.output (TextIO.stdOut, s)
	  } (HTML.HTML{
	    version = NONE,
	    head = [HTML.Head_TITLE name],
	    body = HTML.BODY{
		background = NONE, bgcolor = NONE, text = NONE,
		link = NONE, vlink = NONE, alink = NONE,
		content = HTML.TextBlock(HTMLDev.done ppDev)
	      }
	  });
	TextIO.closeOut (!PP.debugStrm);
	PP.debugStrm := saveStrm
      end;

local
  fun repeat c n = StringCvt.padLeft c n ""
  fun simple1 (name, w, n, openBox) () =
	withPP (name, w) (fn strm => (
	  openBox strm (PP.Rel 0);
	    PP.string strm (repeat #"x" n);
	    PP.cut strm;
	    PP.string strm (repeat #"y" n);
	    PP.cut strm;
	    PP.string strm (repeat #"z" n);
	  PP.closeBox strm))
  fun simple2 (name, w, n, openBox1, openBox2) () =
	withPP (name, w) (fn strm => (
	  openBox1 strm (PP.Rel 0);
	    PP.string strm (repeat #"v" n);
	    PP.cut strm;
	    openBox2 strm (PP.Abs 2);
	      PP.string strm (repeat #"w" n);
	      PP.cut strm;
	      PP.string strm (repeat #"x" n);
	      PP.cut strm;
	      PP.string strm (repeat #"y" n);
	    PP.closeBox strm;
	    PP.cut strm;
	    PP.string strm (repeat #"z" n);
	  PP.closeBox strm))
fun openHBox strm _ = PP.openHBox strm
in
val t01a = simple1 ("Test 01a [hbox]", 10, 2, openHBox)
val t01b = simple1 ("Test 01b [hbox]", 10, 3, openHBox)
val t02a = simple1 ("Test 02a [vbox]", 10, 2, PP.openVBox)
val t02b = simple1 ("Test 02b [vbox]", 10, 3, PP.openVBox)
val t03a = simple1 ("Test 03a [hvbox]", 10, 2, PP.openHVBox)
val t03b = simple1 ("Test 03b [hvbox]", 10, 4, PP.openHVBox)
val t04a = simple1 ("Test 04a [hovbox]", 10, 2, PP.openHOVBox)
val t04b = simple1 ("Test 04b [hovbox]", 10, 4, PP.openHOVBox)
val t05a = simple1 ("Test 05a [box]", 10, 2, PP.openBox)
val t05b = simple1 ("Test 05b [box]", 10, 4, PP.openBox)

val t11a = simple2 ("Test 11a [hbox/hbox]", 10, 2, openHBox, openHBox)
val t11b = simple2 ("Test 11b [hbox/hbox]", 10, 3, openHBox, openHBox)
val t11c = simple2 ("Test 11c [hbox/hbox]", 10, 4, openHBox, openHBox)
val t12a = simple2 ("Test 12a [hbox/vbox]", 10, 2, openHBox, PP.openVBox)
val t12b = simple2 ("Test 12b [hbox/vbox]", 10, 3, openHBox, PP.openVBox)
val t12c = simple2 ("Test 12c [hbox/vbox]", 10, 4, openHBox, PP.openVBox)
val t13a = simple2 ("Test 13a [hbox/hvbox]", 10, 2, openHBox, PP.openHVBox)
val t13b = simple2 ("Test 13b [hbox/hvbox]", 10, 3, openHBox, PP.openHVBox)
val t13c = simple2 ("Test 13c [hbox/hvbox]", 10, 4, openHBox, PP.openHVBox)
val t14a = simple2 ("Test 14a [hbox/hovbox]", 10, 2, openHBox, PP.openHOVBox)
val t14b = simple2 ("Test 14b [hbox/hovbox]", 10, 3, openHBox, PP.openHOVBox)
val t14c = simple2 ("Test 14c [hbox/hovbox]", 10, 4, openHBox, PP.openHOVBox)
val t15a = simple2 ("Test 15a [hbox/box]", 10, 2, openHBox, PP.openBox)
val t15b = simple2 ("Test 15b [hbox/box]", 10, 3, openHBox, PP.openBox)
val t15c = simple2 ("Test 15c [hbox/box]", 10, 4, openHBox, PP.openBox)
val t16a = simple2 ("Test 16a [vbox/hbox]", 10, 2, PP.openVBox, openHBox)
val t16b = simple2 ("Test 16b [vbox/hbox]", 10, 3, PP.openVBox, openHBox)
val t16c = simple2 ("Test 16c [vbox/hbox]", 10, 4, PP.openVBox, openHBox)
val t17a = simple2 ("Test 17a [vbox/vbox]", 10, 2, PP.openVBox, PP.openVBox)
val t17b = simple2 ("Test 17b [vbox/vbox]", 10, 3, PP.openVBox, PP.openVBox)
val t17c = simple2 ("Test 17c [vbox/vbox]", 10, 4, PP.openVBox, PP.openVBox)
val t18a = simple2 ("Test 18a [vbox/hvbox]", 10, 2, PP.openVBox, PP.openHVBox)
val t18b = simple2 ("Test 18b [vbox/hvbox]", 10, 3, PP.openVBox, PP.openHVBox)
val t18c = simple2 ("Test 18c [vbox/hvbox]", 10, 4, PP.openVBox, PP.openHVBox)
val t19a = simple2 ("Test 19a [vbox/hovbox]", 10, 2, PP.openVBox, PP.openHOVBox)
val t19b = simple2 ("Test 19b [vbox/hovbox]", 10, 3, PP.openVBox, PP.openHOVBox)
val t19c = simple2 ("Test 19c [vbox/hovbox]", 10, 4, PP.openVBox, PP.openHOVBox)
val t20a = simple2 ("Test 20a [vbox/box]", 10, 2, PP.openVBox, PP.openBox)
val t20b = simple2 ("Test 20b [vbox/box]", 10, 3, PP.openVBox, PP.openBox)
val t20c = simple2 ("Test 20c [vbox/box]", 10, 4, PP.openVBox, PP.openBox)
val t21a = simple2 ("Test 21a [hvbox/hbox]", 10, 2, PP.openHVBox, openHBox)
val t21b = simple2 ("Test 21b [hvbox/hbox]", 10, 3, PP.openHVBox, openHBox)
val t21c = simple2 ("Test 21c [hvbox/hbox]", 10, 4, PP.openHVBox, openHBox)
val t22a = simple2 ("Test 22a [hvbox/vbox]", 10, 2, PP.openHVBox, PP.openVBox)
val t22b = simple2 ("Test 22b [hvbox/vbox]", 10, 3, PP.openHVBox, PP.openVBox)
val t22c = simple2 ("Test 22c [hvbox/vbox]", 10, 4, PP.openHVBox, PP.openVBox)
val t23a = simple2 ("Test 23a [hvbox/hvbox]", 10, 2, PP.openHVBox, PP.openHVBox)
val t23b = simple2 ("Test 23b [hvbox/hvbox]", 10, 3, PP.openHVBox, PP.openHVBox)
val t23c = simple2 ("Test 23c [hvbox/hvbox]", 10, 4, PP.openHVBox, PP.openHVBox)
val t24a = simple2 ("Test 24a [hvbox/hovbox]", 10, 2, PP.openHVBox, PP.openHOVBox)
val t24b = simple2 ("Test 24b [hvbox/hovbox]", 10, 3, PP.openHVBox, PP.openHOVBox)
val t24c = simple2 ("Test 24c [hvbox/hovbox]", 10, 4, PP.openHVBox, PP.openHOVBox)
val t25a = simple2 ("Test 25a [hvbox/box]", 10, 2, PP.openHVBox, PP.openBox)
val t25b = simple2 ("Test 25b [hvbox/box]", 10, 3, PP.openHVBox, PP.openBox)
val t25c = simple2 ("Test 25c [hvbox/box]", 10, 4, PP.openHVBox, PP.openBox)
val t26a = simple2 ("Test 26a [hovbox/hbox]", 10, 2, PP.openHOVBox, openHBox)
val t26b = simple2 ("Test 26b [hovbox/hbox]", 10, 3, PP.openHOVBox, openHBox)
val t26c = simple2 ("Test 26c [hovbox/hbox]", 10, 4, PP.openHOVBox, openHBox)
val t27a = simple2 ("Test 27a [hovbox/vbox]", 10, 2, PP.openHOVBox, PP.openVBox)
val t27b = simple2 ("Test 27b [hovbox/vbox]", 10, 3, PP.openHOVBox, PP.openVBox)
val t27c = simple2 ("Test 27c [hovbox/vbox]", 10, 4, PP.openHOVBox, PP.openVBox)
val t28a = simple2 ("Test 28a [hovbox/hvbox]", 10, 2, PP.openHOVBox, PP.openHVBox)
val t28b = simple2 ("Test 28b [hovbox/hvbox]", 10, 3, PP.openHOVBox, PP.openHVBox)
val t28c = simple2 ("Test 28c [hovbox/hvbox]", 10, 4, PP.openHOVBox, PP.openHVBox)
val t29a = simple2 ("Test 29a [hovbox/hovbox]", 10, 2, PP.openHOVBox, PP.openHOVBox)
val t29b = simple2 ("Test 29b [hovbox/hovbox]", 10, 3, PP.openHOVBox, PP.openHOVBox)
val t29c = simple2 ("Test 29c [hovbox/hovbox]", 10, 4, PP.openHOVBox, PP.openHOVBox)
val t30a = simple2 ("Test 30a [hovbox/box]", 10, 2, PP.openHOVBox, PP.openBox)
val t30b = simple2 ("Test 30b [hovbox/box]", 10, 3, PP.openHOVBox, PP.openBox)
val t30c = simple2 ("Test 30c [hovbox/box]", 10, 4, PP.openHOVBox, PP.openBox)
val t31a = simple2 ("Test 31a [box/hbox]", 10, 2, PP.openBox, openHBox)
val t31b = simple2 ("Test 31b [box/hbox]", 10, 3, PP.openBox, openHBox)
val t31c = simple2 ("Test 31c [box/hbox]", 10, 4, PP.openBox, openHBox)
val t32a = simple2 ("Test 32a [box/vbox]", 10, 2, PP.openBox, PP.openVBox)
val t32b = simple2 ("Test 32b [box/vbox]", 10, 3, PP.openBox, PP.openVBox)
val t32c = simple2 ("Test 32c [box/vbox]", 10, 4, PP.openBox, PP.openVBox)
val t33a = simple2 ("Test 33a [box/hvbox]", 10, 2, PP.openBox, PP.openHVBox)
val t33b = simple2 ("Test 33b [box/hvbox]", 10, 3, PP.openBox, PP.openHVBox)
val t33c = simple2 ("Test 33c [box/hvbox]", 10, 4, PP.openBox, PP.openHVBox)
val t34a = simple2 ("Test 34a [box/hovbox]", 10, 2, PP.openBox, PP.openHOVBox)
val t34b = simple2 ("Test 34b [box/hovbox]", 10, 3, PP.openBox, PP.openHOVBox)
val t34c = simple2 ("Test 34c [box/hovbox]", 10, 4, PP.openBox, PP.openHOVBox)
val t35a = simple2 ("Test 35a [box/box]", 10, 2, PP.openBox, PP.openBox)
val t35b = simple2 ("Test 35b [box/box]", 10, 3, PP.openBox, PP.openBox)
val t35c = simple2 ("Test 35c [box/box]", 10, 4, PP.openBox, PP.openBox)
end
 
fun t40 () = withPP ("Test 20 [C code]", 20) (fn strm => (
      PP.openHBox strm;
	kw strm "if";
	PP.space strm 1;
	PP.string strm "(x < y)";
	PP.space strm 1;
	PP.string strm "{";
	PP.openHVBox strm (PP.Abs 4);
	  PP.space strm 1;
	  PP.string strm "stmt1;"; PP.space strm 1;
	  PP.openHBox strm;
	    kw strm "if";
	    PP.space strm 1;
	    PP.string strm "(w < z)";
	    PP.space strm 1;
	    PP.string strm "{";
	    PP.openHVBox strm (PP.Abs 4);
	      PP.space strm 1; PP.string strm "stmt2;";
	      PP.space strm 1; PP.string strm "stmt3;";
	      PP.space strm 1; PP.string strm "stmt4;";
	    PP.closeBox strm; PP.newline strm;
	    PP.string strm "}";
	  PP.closeBox strm;
	  PP.space strm 1; PP.string strm "stmt5;";
	  PP.space strm 1; PP.string strm "stmt6;";
	PP.closeBox strm; PP.newline strm;
	PP.string strm "}";
      PP.closeBox strm));

