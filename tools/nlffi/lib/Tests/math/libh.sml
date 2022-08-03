structure LibH = struct
    fun libh s = let
	val sh = DynLinkage.lib_symbol (DynLinkage.main_lib, s)
    in
	fn () => DynLinkage.addr sh
    end
end
