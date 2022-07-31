(* WARNING: This file is generated using 'rwgen wff.gsml' *)

(*#line 1.2 "wff.gsml"*)
structure Wff : WFF = struct

(*#line 4.4 "wff.gsml"*)
   datatype wff =
     FALSE
   | TRUE
   | VAR of string
   | AND of (wff * wff)
   | OR of (wff * wff)
   | NOT of wff

(*#line 9.4 "wff.gsml"*)
   fun simplify e = 
       let fun rewrite'wff redex = 
               (case redex of
                 FALSE => redex
               | TRUE => redex
               | VAR string => redex
               | AND(wff1, wff2) => 
                 (case (rewrite'wff wff1, rewrite'wff wff2) of
                   (TRUE, x) => x
                 | (x, TRUE) => x
                 | (FALSE, x) => FALSE
                 | (x, FALSE) => FALSE
                 | arg => AND arg
                 )
               | OR(wff1, wff2) => 
                 (case (rewrite'wff wff1, rewrite'wff wff2) of
                   (TRUE, x) => TRUE
                 | (x, TRUE) => TRUE
                 | (FALSE, x) => x
                 | (x, FALSE) => x
                 | arg => OR arg
                 )
               | NOT wff => 
                 (case rewrite'wff wff of
                   FALSE => TRUE
                 | TRUE => FALSE
                 | NOT x => x
                 | arg => NOT arg
                 )
               )
       in rewrite'wff e
       end


(*#line 27.4 "wff.gsml"*)
   fun countNots e = 
       let 
(*#line 28.8 "wff.gsml"*)
           val count = ref 0
       in 
          let fun app'wff redex = 
                  let val _ = 
                          (case redex of
                            FALSE => ()
                          | TRUE => ()
                          | VAR string => ()
                          | AND(wff1, wff2) => 
                            ( app'wff wff1; 
                              app'wff wff2 )
                          | OR(wff1, wff2) => 
                            ( app'wff wff1; 
                              app'wff wff2 )
                          | NOT wff => app'wff wff
                          )
                  in 
                     (case redex of
                       NOT _ => count := (( ! count) + 1)
                     | _ => ()
                     )
                  end

          in app'wff e; 
             ! count
          end

       end


(*#line 39.4 "wff.gsml"*)
   fun countNots2 e = 
       let fun fold'wff (redex, foldArg) = 
               let val foldArg = 
                       (case redex of
                         FALSE => foldArg
                       | TRUE => foldArg
                       | VAR string => foldArg
                       | AND(wff1, wff2) => fold'wff (wff2, fold'wff (wff1, foldArg))
                       | OR(wff1, wff2) => fold'wff (wff2, fold'wff (wff1, foldArg))
                       | NOT wff => fold'wff (wff, foldArg)
                       )
               in 
                  (case (redex, foldArg) of
                    (NOT _, n) => n + 1
                  | (_, n) => n
                  )
               end

       in fold'wff (e, 0)
       end


(*#line 48.4 "wff.gsml"*)
   fun allVars e = 
       let fun fold'wff (redex, foldArg) = 
               let val foldArg = 
                       (case redex of
                         FALSE => foldArg
                       | TRUE => foldArg
                       | VAR string => foldArg
                       | AND(wff1, wff2) => fold'wff (wff2, fold'wff (wff1, foldArg))
                       | OR(wff1, wff2) => fold'wff (wff2, fold'wff (wff1, foldArg))
                       | NOT wff => fold'wff (wff, foldArg)
                       )
               in 
                  (case (redex, foldArg) of
                    (VAR v, vs) => v :: vs
                  | (_, vs) => vs
                  )
               end

       in fold'wff (e, [])
       end

end

