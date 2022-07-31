structure TestMatchGen =
struct

local
   structure AstUtil = MDLAstUtil(MDLAst)
   structure AstPP   = MDLAstPrettyPrinter(AstUtil) 
   structure AstRewriter   = MDLAstRewriter(MDLAst) 
   structure MG =
      MatchGen(structure AstPP   = AstPP
               structure AstUtil = AstUtil
               structure AstRewriter = AstRewriter 
              )
   structure MC = MG.MC

   open AstPP AstUtil AstPP.Ast

   fun newDatatype(id,cbs) = DATATYPE(id,[],cbs)
   fun ty id = IDty(IDENT([],id))

   val footy = ty "foo"

   val defs =
       [newDatatype("foo",[CONS("A",SOME(TUPLEty[footy,footy])),
                           CONS("B",NONE),
                           CONS("C",NONE),
                           CONS("D",SOME(RECORDty[("x",footy),("y",footy)]))
                          ]
                   )
       ]
   val info = MG.compileTypes defs

   fun test root rules =  
   let val clauses = map (fn (p, g, x) => CLAUSE([p],g,INTexp(x))) rules
       val _    = print(PP.text(AstPP.exp(CASEexp(root,clauses)))^"\n")
       val dfa  = MG.compile info clauses
       (* val _    = print(MC.toString dfa) *)
       fun fail() = RAISEexp(ID "Match")
       val code = MG.codeGen {root=root, dfa=dfa, fail=fail}
   in  print(PP.text(AstPP.exp code)^"\n")
   end handle MC.MatchCompiler msg => print msg

   fun CONS(x,[])  = CONSpat(IDENT([],x),NONE)
     | CONS(x,[a]) = CONSpat(IDENT([],x),SOME a)
     | CONS(x,xs)  = CONSpat(IDENT([],x),SOME(TUPLEpat xs))

   val WILD = WILDpat

in

   fun rule1() = 
       test
       (ID "B")
       [ (CONS("A",[WILD,WILD]), NONE, 0)
       ]

   fun rule2() =
       test
       (ID "B")
       [ (CONS("A",[WILD,WILD]), NONE, 0),
         (CONS("B",[]), NONE, 1)
       ]

   fun rule3() =
       test
       (ID "B")
       [ (CONS("A",[WILD, CONS("B",[])]), NONE, 0),
         (CONS("A",[CONS("B",[]), WILD]), NONE, 1)
       ]

   fun rule4() =
       test
       (ID "B")
       [ (CONS("A",[CONS("B",[]), CONS("B",[])]), NONE, 0),
         (CONS("A",[IDpat "a", IDpat "b"]), NONE, 1)
       ]

   fun rule5() =
       test
       (ID "B")
       [ (CONS("A",[CONS("B",[]), CONS("B",[])]), NONE, 0),
         (CONS("A",[IDpat "c", CONS("B",[])]), NONE, 1),
         (CONS("A",[IDpat "a", IDpat "b"]), NONE, 2),
         (ASpat("u",CONS("B",[])), NONE, 3)
       ]

   fun rule6() =
       test
       (TUPLEexp[ID "B",ID "C"])
       [ (TUPLEpat[CONS("A",[WILD, WILD]), CONS("B",[])], NONE, 0),
         (TUPLEpat[WILD, WILD], NONE, 1)
       ]

   fun rule7() =
       test
       (ID "B")
       [ (CONS("D",[RECORDpat([("x",IDpat "x"),
                               ("y",CONS("B",[]))],false)]), NONE, 0)
       ]

   fun rule8() =
       test
       (ID "B")
       [ (CONS("D",[RECORDpat([("x",IDpat "x"),("y",CONS("B",[]))],false)]), 
                    SOME(APP("=",TUPLEexp[ID "x", ID "C"])), 0)
       ]
         
   fun rule9() =
       test
       (ID "B")
       [ (CONS("A",[IDpat "x", CONS("B",[])]), 
                  SOME(APP("=",TUPLEexp[ID "x", ID "C"])), 0),
         (CONS("A",[CONS("B",[]), ASpat("z", CONS("C",[]))]), 
                  SOME(APP("=",TUPLEexp[ID "z", ID "C"])), 1),
         (CONS("A",[CONS("B",[]), CONS("C",[])]), NONE, 2),
         (CONS("A",[CONS("B",[]), CONS("B",[])]), NONE, 3),
         (IDpat "z", NONE, 4)
       ]
 
end
end
