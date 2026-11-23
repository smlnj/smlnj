val _ = #set
          (CM.Anchor.anchor "cml")
          (SOME(OS.Path.getParent(OS.FileSys.getDir())));
val _ = CM.autoload "$cml/cml.cm";

(* logging *)
val log = ref ([] : string list)
fun say s = let
      val tid = CML.getTid()
      in
	log := concat[CML.tidToString tid, ": ", s] :: !log
      end
fun run f = (log := []; ignore (RunCML.doit (f, NONE)); rev (!log));

val a = List.app (fn s => print(s^"\n")) (
      run (fn () => let
        val b = Barrier.barrier (fn n => n+1) 0
        val e1 = Barrier.enroll b and e2 = Barrier.enroll b
        val done : unit CML.chan = CML.channel()
        fun thd e () = (
	      say "before wait 1";
              (say ("r1=" ^ Int.toString (Barrier.wait e)))
                handle ex => say ("r1 EXN " ^ exnMessage ex);
	      say "before wait 2";
              (say ("r2=" ^ Int.toString (Barrier.wait e)))
                handle ex => say ("r2 EXN " ^ exnMessage ex);
              CML.send(done, ()))
        in
          CML.spawn (thd e1); CML.spawn (thd e2);
          CML.recv done; CML.recv done;
          RunCML.shutdown OS.Process.success
        end));

