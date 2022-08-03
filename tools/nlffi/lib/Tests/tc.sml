local
    val l = DL.dlopen (SOME "./testcall.so", true, true)
    fun sym s = DL.dlsym (l, s)
in
    val [tc1, tc2, tc3, tc4, dummy, click, show, gen, mmalloc] =
	map sym ["tc1", "tc2", "tc3", "tc4",
                 "dummy", "click", "show", "gen", "mmalloc"]
end
