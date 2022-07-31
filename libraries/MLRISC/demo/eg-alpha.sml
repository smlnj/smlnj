(*---------------------------------------------------------------------------
 * Here's an example: factorial for alpha.
 *---------------------------------------------------------------------------*)
fun eg2() = (* fact(n) *)
let open AlphaBackEnd
    val fact  = C.newReg()
    val i     = C.newReg()
    val n     = C.GPReg 0  (* let's say r0 is the input/output parameter *)
    val loop  = Label.newLabel "loop"
    val exit  = Label.newLabel "exit"
in  codegen
      (Label.newLabel "factorial",
       [T.MV(64, i,    T.REG(64, n)),
        T.MV(64, fact, T.LI 1),
        T.DEFINE loop,
        T.BCC(T.CMP(64, T.LE, T.REG(64, n), T.LI 0), exit),
        T.MV(64, fact, T.MULS(64, T.REG(64, i), T.REG(64, fact))),
        T.MV(64, i,    T.SUB(64, T.REG(64, i), T.LI 1)),
        T.JMP(T.LABEL(LabelExp.LABEL loop), []),
        T.DEFINE exit,
        T.MV(64, n, T.REG(64, fact)),
        T.RET []
       ]
      )
end
