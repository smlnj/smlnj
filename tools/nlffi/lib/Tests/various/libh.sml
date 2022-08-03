structure LibH = struct
    local 
	val lh = DynLinkage.open_lib
		    { name = "./various.so", global = true, lazy = true }
    in
        fun libh s = let
	    val sh = DynLinkage.lib_symbol (lh, s)
	in
	    fn () => DynLinkage.addr sh
	end
    end
end
