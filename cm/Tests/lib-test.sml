structure LibTest = struct
    structure StringMap = BinaryMapFn
	(struct
	    type ord_key = string
	    val compare = String.compare
	end)

    val _ = print "hello, world!\n"
end
