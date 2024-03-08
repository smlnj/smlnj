fun timeit file = let
      val t0 = Time.now()
      val json = JSONParser.parseFile file
      in
	print(concat[Time.toString(Time.-(Time.now(), t0)), " seconds\n"])
(*
        (Time.toReal(Time.-(Time.now(), t0)), json)
*)
      end;

