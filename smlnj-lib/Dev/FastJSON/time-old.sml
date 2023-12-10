fun timeit file = let
      val t0 = Time.now()
      val json = JSONParser.parseFile file
      in
        (Time.toReal(Time.-(Time.now(), t0)), json)
      end;

