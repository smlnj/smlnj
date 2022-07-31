   structure BM =
   struct
      structure W8A = Word8Array
      structure W8  = Word8
      structure W   = Word
      fun create N = W8A.array(W.toIntX(W.>>(W.fromInt(N + 7), 0w3)), 0w0) 
      fun markAndTest(bitmap,i) = 
      let val byte = W.toIntX(W.>>(W.fromInt i,0w3))
          val mask = W8.<<(0w1, W.andb(W.fromInt i,0w7))
          val n    = W8A.sub(bitmap, byte)
      in  W8.andb(n, mask) <> 0w0 orelse
          (W8A.update(bitmap, byte, W8.orb(n, mask)); false)
      end
      fun contains(bitmap,i) = 
      let val byte = W.toIntX(W.>>(W.fromInt i,0w3))
          val mask = W8.<<(0w1, W.andb(W.fromInt i,0w7))
      in  W8.andb(W8A.sub(bitmap, byte), mask) <> 0w0 end
   end
