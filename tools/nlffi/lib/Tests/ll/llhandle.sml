structure LLHandle = struct
    local 
	val lh = DynLinkage.open_lib
		    { name = "./ll.so", global = true, lazy = true }
    in
        fun ll s = let
	    val sh = DynLinkage.lib_symbol (lh, s)
	in
	    fn () => DynLinkage.addr sh
	end
    end
end
