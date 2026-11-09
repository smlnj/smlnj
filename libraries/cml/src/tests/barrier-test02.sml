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
        val bar = Barrier.barrier (fn n => n+1) 0
        val e1 = Barrier.enroll bar
        val e2 = Barrier.enroll bar
        val e3 = Barrier.enroll bar
        val done : unit CML.chan = CML.channel()
        fun waitTimeout () = CML.select [
                CML.wrap(CML.recvEvt done, fn _ => ()),
                CML.wrap(CML.timeOutEvt (Time.fromSeconds 3), fn _ => say "TIMEOUT")
              ]
        in
          CML.spawn (fn () => (
            Barrier.resign e3; say "e3 resigned";
            CML.send(done, ())));
          CML.spawn (fn () => (
            say ("e1 got " ^ Int.toString(Barrier.wait e1));
            CML.send(done, ())));
          CML.spawn (fn () => (
            say ("e2 got " ^ Int.toString(Barrier.wait e2));
            CML.send(done, ())));
          waitTimeout(); waitTimeout(); waitTimeout();
          RunCML.shutdown OS.Process.success
        end));
