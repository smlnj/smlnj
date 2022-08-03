structure MemAccess64 = struct

    fun load2 a = (RawMemInlineT.w32l (a+0w4), RawMemInlineT.w32l a)
    fun store2 (a, (hi, lo)) =
	(RawMemInlineT.w32s (a+0w4, hi); RawMemInlineT.w32s (a, lo))
end
