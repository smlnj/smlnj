functor CFGViewer
   (structure CFG : CONTROL_FLOW_GRAPH
    structure GraphViewer : GRAPH_VIEWER
    structure Asm	  : INSTRUCTION_EMITTER where I = CFG.I)
      : sig
	    val view : CFG.cfg -> unit
	end =
struct
   structure CFG = CFG
   structure L = GraphLayout
   structure FMT = FormatInstruction(Asm)
   structure G = Graph

   val outline = MLRiscControl.getFlag "view-outline"

   fun view(cfg as G.GRAPH g) = let
       val CFG.INFO{annotations, ...} = #graph_info g
       val toString = FMT.toString (!annotations)
       fun graph _ = []
       val colorScale = 
	   Array.fromList
	     ["#ccffff", "#99ffff", "#66ccff", "#54a9ff", "#ccff99", 
	      "#ffff99", "#ffcc66", "#ff9966", "#cc6666", "#d14949",
	      "#d14949"]

       fun weightRange([], min, max) = (min, max-min)
	 | weightRange((_,_,CFG.EDGE{w, ...})::rest, min, max) = let
	     val wt = !w
           in
	     if wt > max then weightRange(rest, min, wt)
	     else if wt < min then weightRange(rest, wt, max)
	     else weightRange(rest, min, max)
           end

       val (loWt, range) = weightRange( #edges g (), ~1.0, ~1.0)

       fun color w = let
	   val pos = 
	       if range < 100.0 
	       then floor(((w-loWt) * 10.0) / range)
	       else floor (Math.log10(w-loWt) * 10.0 / Math.log10 range)
       in
	   Array.sub(colorScale, pos)
       end

       val ENTRY = hd(#entries g ())
       val EXIT  = hd(#exits g ())

       val red = L.COLOR "#ff0000" 
       val yellow = L.COLOR "yellow"
       val green = L.COLOR "green"

       fun edge(i,j,CFG.EDGE{w, ...}) = 
       let val label = L.LABEL(Real.toString (!w))
       in  [label, L.COLOR(color(!w))] 
       end

       fun title(blknum,ref freq) = 
           " "^Int.toString blknum ^ " freq="^Real.toString freq

       fun ann(annotations) = 
            List.foldl(fn (a,l) => "/* "^Annotations.toString a^" */\n"^l) ""
                             (!annotations)

       fun node(_, CFG.BLOCK{kind, labels, id, freq, insns, annotations, ...}) = 
	 (case kind
	   of CFG.START => 
	        [L.LABEL("entry"^title(id,freq)^"\n"^ann(annotations))]
	    | CFG.STOP  => 
                [L.LABEL("exit"^title(id,freq))]
	    | _ => 
              [L.LABEL("BLK"^title(id,freq)^"\n"^
		 (case !labels
		   of [] => ""
		    | labs => 
		       String.concatWith ":\n" (map Label.toString labs) ^ ":\n"
                 (*easc*)) ^
                 ann(annotations)^
                 (if !outline then "" else
                 List.foldl 
		     (fn (i,t) => let val text = toString i
				  in  if text = "" then t else text^"\n"^t 
				  end) 
		     "" 
		     (!insns)))]
	  (*esac*))

   in  
      GraphViewer.view
           (L.makeLayout{graph=graph, edge=edge, node=node} cfg)
   end

end
