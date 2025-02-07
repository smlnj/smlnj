(* WARNING: this is generated by running 'nowhere alphaPeephole.peep'.
 * Do not edit this file directly.
 * Version 1.2.2
 *)

(*#line 8.1 "alphaPeephole.peep"*)
functor AlphaPeephole(
(*#line 9.5 "alphaPeephole.peep"*)
                      structure Instr : ALPHAINSTR

(*#line 10.5 "alphaPeephole.peep"*)
                      structure Eval : MLTREE_EVAL

(*#line 11.7 "alphaPeephole.peep"*)
                      sharing Instr.T = Eval.T
                     ): PEEPHOLE =
struct

(*#line 14.4 "alphaPeephole.peep"*)
   structure I = Instr

(*#line 15.4 "alphaPeephole.peep"*)
   structure CB = CellsBasis

(*#line 18.4 "alphaPeephole.peep"*)
   fun peephole instrs =
       let
(*#line 19.8 "alphaPeephole.peep"*)
           fun isZero (I.LABop le) = (((Eval.valueOf le) = 0) handle _ => false
)
             | isZero (I.REGop r) = (CB.registerNum r) = 31
             | isZero (I.IMMop i) = i = 0
             | isZero _ = false

(*#line 24.8 "alphaPeephole.peep"*)
           fun removable p_0 =
               let val v_12 = p_0
                   fun state_7 () = false
                   fun state_4 (v_0, v_1, v_2) =
                       let val ra = v_0
                           and rb = v_1
                           and rc = v_2
                       in (CB.sameColor (ra, rc)) andalso (isZero rb)
                       end
               in
                  let val v_11 = v_12
                  in
                     (case v_11 of
                       I.ANNOTATION v_8 =>
                       let val {a=v_10, i=v_9, ...} = v_8
                       in
                          let val a = v_10
                              and i = v_9
                          in removable i
                          end
                       end
                     | I.INSTR v_8 =>
                       (case v_8 of
                         I.LDA v_4 =>
                         let val {b=v_7, d=v_6, r=v_5, ...} = v_4
                         in
                            let val b = v_7
                                and d = v_6
                                and r = v_5
                            in (isZero d) andalso (CB.sameColor (r, b))
                            end
                         end
                       | I.OPERATE v_4 =>
                         let val {oper=v_3, ra=v_0, rb=v_1, rc=v_2, ...} = v_4
                         in
                            (case v_3 of
                              I.ADDQ => state_4 (v_0, v_1, v_2)
                            | I.SUBQ => state_4 (v_0, v_1, v_2)
                            | _ => state_7 ()
                            )
                         end
                       | _ => state_7 ()
                       )
                     | _ => state_7 ()
                     )
                  end
               end

(*#line 31.8 "alphaPeephole.peep"*)
           fun symmetric (I.STQ, I.LDQ) = true
             | symmetric (I.STL, I.LDL) = true
             | symmetric (I.STW, I.LDW) = true
             | symmetric (I.STB, I.LDB) = true
             | symmetric _ = false

(*#line 37.8 "alphaPeephole.peep"*)
           fun sameOperand (I.REGop r1, I.REGop r2) = CB.sameColor (r1, r2)
             | sameOperand (I.IMMop i1, I.IMMop i2) = i1 = i2
             | sameOperand (I.LABop l1, I.LABop l2) = (((Eval.valueOf l1) = (Eval.valueOf l2)) handle _ => false
)
             | sameOperand _ = false

(*#line 43.8 "alphaPeephole.peep"*)
           fun loop (current, instrs) =
               let val v_31 = current
                   fun state_7 (v_13, v_14) =
                       let val i = v_13
                           and rest = v_14
                       in (if (removable i)
                             then (loop (rest, instrs))
                             else
                             let val i = v_13
                                 and rest = v_14
                             in loop (rest, i :: instrs)
                             end)
                       end
               in
                  (case v_31 of
                    op :: v_30 =>
                    let val (v_13, v_14) = v_30
                    in
                       (case v_13 of
                         I.INSTR v_29 =>
                         (case v_29 of
                           I.STORE v_28 =>
                           let val {b=v_24, d=v_22, r=v_18, stOp=v_15, ...} = v_28
                           in
                              (case v_14 of
                                op :: v_27 =>
                                let val (v_20, v_16) = v_27
                                in
                                   (case v_20 of
                                     I.INSTR v_26 =>
                                     (case v_26 of
                                       I.LOAD v_25 =>
                                       let val {b=v_23, d=v_21, ldOp=v_19, r=v_17, ...} = v_25
                                       in
                                          let val b1 = v_24
                                              and b2 = v_23
                                              and d1 = v_22
                                              and d2 = v_21
                                              and ld = v_20
                                              and ldOp = v_19
                                              and r1 = v_18
                                              and r2 = v_17
                                              and rest = v_16
                                              and st = v_13
                                              and stOp = v_15
                                          in (if ((((symmetric (stOp, ldOp)) andalso (CB.sameColor (r1, r2))) andalso (CB.sameColor (b1, b2))) andalso (sameOperand (d1,
                                                d2)))
                                                then (loop (rest, st :: instrs))
                                                else (state_7 (v_13, v_14)))
                                          end
                                       end
                                     | _ => state_7 (v_13, v_14)
                                     )
                                   | _ => state_7 (v_13, v_14)
                                   )
                                end
                              | nil => state_7 (v_13, v_14)
                              )
                           end
                         | _ => state_7 (v_13, v_14)
                         )
                       | _ => state_7 (v_13, v_14)
                       )
                    end
                  | nil => instrs
                  )
               end
       in loop (instrs, [])
       end
end

