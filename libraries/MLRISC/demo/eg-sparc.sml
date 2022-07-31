(*---------------------------------------------------------------------------
 * Here's an example: factorial for sparc.
 *---------------------------------------------------------------------------*)
fun eg1() = (* fact(n) *)
let open SparcBackEnd
    val fact  = C.newReg()
    val i     = C.newReg()
    val n     = C.GPReg 1  (* let's say r1 is the input/output parameter *)
    val loop  = Label.newLabel "loop"
    val exit  = Label.newLabel "exit"
in  codegen
      (Label.newLabel "factorial",
       [T.MV(32, i,    T.REG(32, n)),
        T.MV(32, fact, T.LI 1),
        T.DEFINE loop,
        T.BCC(T.CMP(32, T.LE, T.REG(32, n), T.LI 0), exit),
        T.MV(32, fact, T.MULS(32, T.REG(32, i), T.REG(32, fact))),
        T.MV(32, i,    T.SUB(32, T.REG(32, i), T.LI 1)),
        T.JMP(T.LABEL(LabelExp.LABEL loop), []),
        T.DEFINE exit,
        T.MV(32, n, T.REG(32, fact)),
        T.RET []
       ]
      )
end
