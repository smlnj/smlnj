(* Copyright 2003 by The SML/NJ Fellowship *)
(* basics/pputil.sml *)

structure PPUtil : PPUTIL =
struct

  structure S : SYMBOL = Symbol
  structure PP = PrettyPrint

  val pps = PP.string

  datatype break_style = CONSISTENT | INCONSISTENT

  fun openStyleBox style =
      case style
        of CONSISTENT => PP.openHVBox
         | INCONSISTENT => PP.openHOVBox

(* pretty print a separator followed by a cut *)
  fun sepWithCut sep ppstream = (PP.string ppstream sep; PP.cut ppstream)

(* pretty print a separator followed by a space *)
  fun sepWithSpc sep ppstream = (PP.string ppstream sep; PP.space ppstream 1)

  fun ppSequence0 ppstream (sep:PP.stream->unit,pr,elems) =
      let fun prElems [] = ()
	    | prElems [el] = pr ppstream el
	    | prElems (el::rest) =
	        (pr ppstream el;
		 sep ppstream;
                 prElems rest)
       in prElems elems
      end

  fun ppSequence ppstream {sep:PP.stream->unit, pr:PP.stream->'a->unit,
                           style:break_style} (elems: 'a list) =
      (openStyleBox style ppstream (PP.Abs 0); (* was `Rel 0` *)
       ppSequence0 ppstream (sep,pr,elems);
       PP.closeBox ppstream)

  fun ppClosedSequence ppstream{front:PP.stream->unit,sep:PP.stream->unit,
                                back:PP.stream->unit,pr:PP.stream->'a->unit,
                                style:break_style} (elems:'a list) =
      (PP.openHVBox ppstream (PP.Rel 1);
       front ppstream;
       openStyleBox style ppstream (PP.Abs 0); (* was `Rel 0` *)
       ppSequence0 ppstream (sep,pr,elems);
       PP.closeBox ppstream;
       back ppstream;
       PP.closeBox ppstream)

  fun ppBracketedSequence (front: string, back: string, ppElem) ppstrm elems =
      (PP.openHBox ppstrm;
       PP.string ppstrm front;
       ppSequence0 ppstrm ((fn ps => PP.string ps ","), ppElem, elems);
       PP.string ppstrm back;
       PP.closeBox ppstrm)

  fun ppSym ppstream (s:S.symbol) = PP.string ppstream (S.name s)

  fun ppString ppstream = PP.string ppstream o PrintUtil.formatString

  fun ppvseqNoBox ppstream pr_elem elems =
      let fun prElems [el] = pr_elem ppstream el
	    | prElems (el::rest) = (pr_elem ppstream el;
				    PP.cut ppstream;
                                    prElems rest)
	    | prElems [] = ()
       in prElems elems
      end

  fun ppvseq ppstream indent (sep:string) pr_elem elems =
      let fun prElems [el] = pr_elem ppstream el
	    | prElems (el::rest) = (pr_elem ppstream el;
                                    PP.string ppstream sep;
				    PP.cut ppstream;
                                    prElems rest)
	    | prElems [] = ()
      in PP.openVBox ppstream (PP.Abs indent);
	   PP.cut ppstream;
	   prElems elems;
         PP.closeBox ppstream
      end

  fun ppvlist ppstrm (header,separator,pr_elem,elems) =
      case elems
	of nil => ()
	 | first::rest =>
	     (PP.string ppstrm header;
	      pr_elem ppstrm first;
	      app (fn x => (PP.cut ppstrm;
			    PP.string ppstrm separator;
			    pr_elem ppstrm x))
		   rest)

  fun ppvlist' ppstrm (header,separator,pr_elem,elems) =
      case elems
	of nil => ()
	 | first::rest =>
	     (pr_elem ppstrm header first;
	      app (fn x => (PP.newline ppstrm;
			    pr_elem ppstrm separator x))
		   rest)

(* these don't belong here: ppIntPath not used, ppSympath, ppInvPath too specialized,
 and reverse introduce dependence on ElabData/basics/sympath.sml.
  (* debug print functions *)
  fun ppIntPath ppstream =
      ppClosedSequence ppstream
	{front=(fn pps => PP.string pps "["),
	 sep=(fn pps => (PP.string pps ","; PP.break pps {nsp=0,offset=0})),
	 back=(fn pps => PP.string pps "]"),
	 style=INCONSISTENT,
	 pr=(fn pps => PP.string pps o Int.toString)}

  fun ppSymPath ppstream (path: SymPath.path) =
      PP.string ppstream (SymPath.toString path)

  fun ppInvPath ppstream (rpath: InvPath.path) =
      PP.string ppstream (InvPath.toString rpath)
*)

  fun ppi ppstrm (i:int) = pps ppstrm (Int.toString i)

  fun ppcomma ppstrm = pps ppstrm ","

  fun ppcomma_nl ppstrm  = (ppcomma ppstrm; PP.newline ppstrm)

(* this function is bogus; its uses should be replaced by better boxes *)
  fun nl_indent ppstrm i =
      PP.break ppstrm {nsp=1000,offset=i}

  fun nl_app ppstrm f =
      let fun g [] = ()
	    | g [el] = f ppstrm el
	    | g (el::rst) = (f ppstrm el; PP.newline ppstrm; g rst)
       in g
      end

  fun br_app ppstrm f =
      let fun g [] = ()
	    | g [el] = f ppstrm el
	    | g (el::rst) = (f ppstrm el; PP.space ppstrm 1; g rst)
       in g
      end

  fun en_pp ppstrm =
      {openVBox = (fn indent => PP.openVBox ppstrm (PP.Abs indent)),  (* vertical *)
       openHVBox = (fn indent => PP.openHVBox ppstrm (PP.Abs indent)),  (* CONSISTENT *)
       openHOVBox = (fn indent => PP.openHOVBox ppstrm (PP.Abs indent)),  (* INCONSISTENT *)
       closeBox = fn () => PP.closeBox ppstrm,
       pps = pps ppstrm,
       ppi = ppi ppstrm,
       break = fn nsp_offset => PP.break ppstrm nsp_offset,
       newline = fn () => PP.newline ppstrm};

  fun ppArray ppstrm (f:PP.stream -> 'a -> unit, a:'a array) =
      let val {openHVBox,openHOVBox,pps,break,closeBox,...} = en_pp ppstrm
	  fun loop i =
	      let val elem = Array.sub(a,i)
	       in pps (Int.toString i);
		  pps ":";
		  break {nsp=1,offset=0};
		  f ppstrm elem;
		  break {nsp=1,offset=0};
		  loop (i+1)
	      end
       in openHOVBox 0;
	  loop 0 handle General.Subscript => ();
	  closeBox()
      end

  fun C f x y = f y x;

  fun ppTuple ppstrm f = ppClosedSequence ppstrm {
	  front = C pps "(", sep = sepWithCut ",", back = C pps ")",
	  pr = f, style = INCONSISTENT
        }

end (* structure PPUtil *)
