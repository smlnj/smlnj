(* load the word list *)
val wl = let
      val inS = TextIO.openIn "word-list.txt"
      fun lp words = (case TextIO.inputLine inS
             of NONE => List.rev words
              | SOME "\n" => lp words
              | SOME ln => lp(String.substring(ln, 0, size ln - 1) :: words)
            (* end case *))
      in
        lp [] before TextIO.closeIn inS
      end;

fun search target d = let
      fun search' ([], candidates) = List.rev candidates
        | search' (w::ws, candidates) = if (LevDistance.editDistance(target, w) <= d)
            then search'(ws, w::candidates)
            else search'(ws, candidates)
      in
        search' (wl, [])
      end;

fun timeit target d = let
      val t0 = Time.now()
      val candidates = search target d
      val dt = Time.-(Time.now(), t0)
      in
        (Time.toReal dt, candidates)
      end;
