structure PDBHandle = struct
    local 
	val lh = DynLinkage.open_lib
		    { name = "./pdb.so", global = true, lazy = true }
    in
        fun pdb s = let
	    val sh = DynLinkage.lib_symbol (lh, s)
	in
	    fn () => DynLinkage.addr sh
	end
    end
end
