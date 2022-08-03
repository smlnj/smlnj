structure Printf = struct
    val printf = F_printf.f
    fun printf_int_int (f, i1, i2) = let
	val f' = ZString.dupML' f
    in
	C.Cvt.ml_sint (C.call' T_printf_int_int.typ
			       (C.U.fcast (C.Light.fptr (F_printf.fptr ())),
				(f', C.Cvt.c_sint i1, C.Cvt.c_sint i2)))
	before C.free' f'
    end
    fun printf_int_double (f, i, d) = let
	val f' = ZString.dupML' f
    in
	C.Cvt.ml_sint (C.call' T_printf_int_double.typ
			       (C.U.fcast (C.Light.fptr (F_printf.fptr ())),
				(f', C.Cvt.c_sint i, C.Cvt.c_double d)))
	before C.free' f'
    end
    fun printf_int_string_pointer (f, i, s, p) = let
	val f' = ZString.dupML' f
	val s' = ZString.dupML' s
    in
	C.Cvt.ml_sint (C.call' T_printf_int_string_pointer.typ
			       (C.U.fcast (C.Light.fptr (F_printf.fptr ())),
				(f', C.Cvt.c_sint i, s', C.Ptr.inject' p)))
	before (C.free' f'; C.free' s')
    end
end
