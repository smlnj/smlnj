    exception InvalidUTF8
    exception IncompleteUTF8

    (* a simple state machine for getting a valid UTF8 byte sequence.
     * See https://unicode.org/mail-arch/unicode-ml/y2003-m02/att-0467/01-The_Algorithm_to_Valide_an_UTF-8_String
     * for a description of the state machine.
     *)
    fun getUTF8 (getc : (char, 'a) StringCvt.reader) (chr0, byte0, inS : 'a) = let
          fun getByte inS = (case getc inS
                 of SOME(c, inS') => (Word.fromInt(ord c), c, inS')
                  | NONE => raise IncompleteUTF8
                (* end case *))
          fun return (chrs, inS) = (chr0 :: rev chrs, inS)
          fun inRange (minB : word, b, maxB) = ((b - minB) <= maxB - minB)
          (* handles last byte for all multi-byte sequences *)
          fun stateA (inS, chrs) = let
                val (b, c, inS) = getByte inS
                in
                  if inRange(0wx80, b, 0wxbf)
                    then return (inS, c::chrs)
                    else raise InvalidUTF8
                end
          (* handles second/third byte for three/four-byte sequences *)
          and stateB (inS, chrs) = let
                val (b, c, inS) = getByte inS
                in
                  if inRange(0wx80, b, 0wxbf)
                    then stateA (inS, c::chrs)
                    else raise InvalidUTF8
                end
          (* byte0 = 0b1110_0000 (3-byte sequence) *)
          and stateC (inS, chrs) = let
                val (b, c, inS) = getByte inS
                in
                  if inRange(0wxa0, b, 0wxbf)
                    then stateA (inS, c::chrs)
                    else raise InvalidUTF8
                end
          (* byte0 = 0b1110_1101 (3-byte sequence) *)
          and stateD (inS, chrs) = let
                val (b, c, inS) = getByte inS
                in
                  if inRange(0wx80, b, 0wx9f)
                    then stateA (inS, c::chrs)
                    else raise InvalidUTF8
                end
          (* byte0 = 0b1111_0001 .. 0b1111_0011 (4-byte sequence) *)
          and stateE (inS, chrs) = let
                val (b, c, inS) = getByte inS
                in
                  if inRange(0wx80, b, 0wxbf)
                    then stateB (inS, c::chrs)
                    else raise InvalidUTF8
                end
          (* byte0 = 0b1111_0000 (4-byte sequence) *)
          and stateF (inS, chrs) = let
                val (b, c, inS) = getByte inS
                in
                  if inRange(0wx90, b, 0wxbf)
                    then stateB (inS, c::chrs)
                    else raise InvalidUTF8
                end
          (* byte0 = 0b1111_1000 (4-byte sequence) *)
          and stateG (inS, chrs) = let
                val (b, c, inS) = getByte inS
                in
                  if inRange(0wx80, b, 0wx8f)
                    then stateB (inS, c::chrs)
                    else raise InvalidUTF8
                end
          in
            if (byte0 <= 0wx7f)
              then ([chr0], inS) (* an ASCII character, so this should not happen *)
            else if inRange(0wxc2, byte0, 0wxdf)
              then stateA (inS, [])
            else if inRange(0wxe1, byte0, 0wxec) orelse inRange(0wxee, byte0, 0wxef)
              then stateB (inS, [])
            else if (byte0 = 0wxe0)
              then stateC (inS, [])
            else if (byte0 = 0wxed)
	      then stateD (inS, [])
            else if inRange(0wxf1, byte0, 0wxf3)
              then stateE (inS, [])
            else if (byte0 = 0wxf0)
	      then stateF (inS, [])
            else if (byte0 = 0wxf4)
	      then stateG (inS, [])
              else raise InvalidUTF8
          end
