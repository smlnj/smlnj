use "json.sml";
use "fast-parser.sml";

fun timeFast file = let
      val t0 = Time.now()
      val inS = TextIO.getInstream(TextIO.openIn file)
      val (json, _) = FastJSONParser.parse TextIO.StreamIO.input1 inS
      val _ = TextIO.StreamIO.closeIn inS;
      in
	print(concat[Time.toString(Time.-(Time.now(), t0)), " seconds\n"])
(*
        (Time.-(Time.now(), t0), json)
*)
      end;
