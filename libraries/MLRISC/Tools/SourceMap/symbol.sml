structure UniqueSymbol :> UNIQUE_SYMBOL =
struct

   structure H = HashTable

   datatype symbol = SYMBOL of string ref * word

   fun equal(SYMBOL(a,_),SYMBOL(b,_)) = a = b
   fun compare(SYMBOL(a,_),SYMBOL(b,_)) = String.compare(!a,!b)
   fun hash(SYMBOL(_,w)) = w
   fun toString(SYMBOL(s,_)) = !s

   exception NotThere

   fun hashIt(SYMBOL(ref s,_)) = HashString.hashString s
   fun eq(SYMBOL(ref x,a),SYMBOL(ref y,b)) = a = b andalso x = y

   val table = H.mkTable (hashIt,eq) (117,NotThere) 
                  : (symbol,symbol) H.hash_table

   val lookup = H.lookup table
   val insert = H.insert table

   fun fromString name = 
       let val s = SYMBOL(ref name,HashString.hashString name) 
       in  lookup s handle _ => (insert(s,s); s) end
end
