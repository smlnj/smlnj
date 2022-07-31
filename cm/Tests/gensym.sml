structure GenSym = struct
    local
	val c = ref 0
    in
	fun gensym () = let
	    val n = !c
	in
	    c := n + 1;
	    n
	end
    end
end
