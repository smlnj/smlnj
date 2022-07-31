structure Vararg =
  struct

  type addr = VarargConstants.W.word

   datatype arg
     = SINT_ARG of int
     | DOUBLE_ARG of real
     | PTR_ARG of addr
     | STRING_ARG of string

  end
