(*
 * Process rtl descriptions
 *)
functor MDLRTLTools
   (structure AstUtil   : MDL_AST_UTIL
    structure MLTreeRTL : MLTREE_RTL
   ) : MDL_RTL_TOOLS =
struct
   structure Ast = AstUtil.Ast
   structure RTL = MLTreeRTL
   structure T   = RTL.T
   structure A   = Ast
   structure U   = AstUtil
   structure CellsBasis = CellsBasis

   fun error msg = MLRiscErrorMsg.error("MDRTLTools",msg)

   (*========================================================================
    *
    * Simplify an RTL expression
    *
    *========================================================================*)
   fun simplify rtl =
   let fun stm reduce (T.SEQ [s]) = s
         | stm reduce (T.IF(T.TRUE, y, n)) = y
         | stm reduce (T.IF(T.FALSE, y, n)) = n
         | stm reduce s = s

       and (* rexp reduce (T.ADD(_,T.LI 0,x)) = x
         | rexp reduce (T.ADD(_,x,T.LI 0)) = x
         | rexp reduce (T.SUB(_,x,T.LI 0)) = x
         | rexp reduce (T.MULS(_,_,zero as T.LI 0)) = zero
         | rexp reduce (T.MULU(_,_,zero as T.LI 0)) = zero
         | rexp reduce (T.MULT(_,_,zero as T.LI 0)) = zero
         | rexp reduce (T.MULS(_,zero as T.LI 0, _)) = zero
         | rexp reduce (T.MULU(_,zero as T.LI 0, _)) = zero
         | rexp reduce (T.MULT(_,zero as T.LI 0, _)) = zero
         | rexp reduce (T.MULS(_,x,T.LI 1)) = x
         | rexp reduce (T.MULU(_,x,T.LI 1)) = x
         | rexp reduce (T.MULT(_,x,T.LI 1)) = x
         | rexp reduce (T.DIVS(_,x,T.LI 1)) = x
         | rexp reduce (T.DIVU(_,x,T.LI 1)) = x
         | rexp reduce (T.DIVT(_,x,T.LI 1)) = x
         | rexp reduce (T.ANDB(_,_,zero as T.LI 0)) = zero
         | rexp reduce (T.ANDB(_,zero as T.LI 0,_)) = zero
         | *) rexp reduce (e as T.ANDB(_,x,y)) =
             if RTL.Util.eqRexp(x,y) then x else e
         (* | rexp reduce (T.ORB(_,x,T.LI 0)) = x
         | rexp reduce (T.ORB(_,T.LI 0,x)) = x *)
         | rexp reduce (e as T.ORB(_,x,y)) =
             if RTL.Util.eqRexp(x,y) then x else e
         | rexp reduce (T.NOTB(_,T.NOTB(_,x))) = x
         | rexp reduce (e as T.SX(t1,t2,x)) = if t1 = t2 then x else e
         | rexp reduce (e as T.ZX(t1,t2,x)) = if t1 = t2 then x else e
         | rexp reduce e = e
       and fexp reduce e = e

       and ccexp reduce (T.NOT T.TRUE) = T.FALSE
         | ccexp reduce (T.NOT T.FALSE) = T.TRUE
         | ccexp reduce (T.AND(T.FALSE,_)) = T.FALSE
         | ccexp reduce (T.AND(_,T.FALSE)) = T.FALSE
         | ccexp reduce (T.AND(T.TRUE, x)) = x
         | ccexp reduce (T.AND(x,T.TRUE)) = x
         | ccexp reduce (T.OR(T.FALSE,x)) = x
         | ccexp reduce (T.OR(x,T.FALSE)) = x
         | ccexp reduce (T.OR(T.TRUE, _)) = T.TRUE
         | ccexp reduce (T.OR(_,T.TRUE)) = T.TRUE
         | ccexp reduce (e as T.CMP(_,T.EQ,x,y)) =
            if RTL.Util.eqRexp(x,y) then T.TRUE else e
         | ccexp reduce (e as T.CMP(_,T.NE,x,y)) =
            if RTL.Util.eqRexp(x,y) then T.FALSE else e
         | ccexp reduce e = e

       val rewriter =
             RTL.Rewrite.rewrite{rexp=rexp, fexp=fexp, ccexp=ccexp, stm=stm}
   in  #stm rewriter rtl
   end

   (*========================================================================
    *
    * Translate an RTL into something else
    *
    *========================================================================*)
   fun transRTL
        {app,id,int,intinf,word32,string,list,Nil,tuple,record,arg,
         cellkind,oper,region}
                rtl =
   let fun word w = word32(Word32.fromLargeWord(Word.toLargeWord w))
       fun ternOp n (x,ty,y,z) = app(n,[x, int ty, rexp y, rexp z])
       and binOp n (ty,x,y) = app(n,[int ty,rexp x,rexp y])
       and unaryOp n (ty,x) = app(n,[int ty,rexp x])
       and rexp(T.LI i) = app("LI",[intinf i])
         | rexp(T.NEG x) = unaryOp "NEG" x
         | rexp(T.ADD x) = binOp "ADD" x
         | rexp(T.SUB x) = binOp "SUB" x
         | rexp(T.MULS x) = binOp "MULS" x
(* FIXME
         | rexp(T.DIVS x) = ternOp "DIVS" x
         | rexp(T.REMS x) = ternOp "REMS" x
 *)
         | rexp(T.MULU x) = binOp "MULU" x
         | rexp(T.DIVU x) = binOp "DIVU" x
         | rexp(T.REMU x) = binOp "REMU" x
         | rexp(T.NEGT x) = unaryOp "NEGT" x
         | rexp(T.ADDT x) = binOp "ADDT" x
         | rexp(T.SUBT x) = binOp "SUBT" x
         | rexp(T.MULT x) = binOp "MULT" x
(* FIXME
         | rexp(T.DIVT x) = ternOp "DIVT" x
 *)
         | rexp(T.NOTB x) = unaryOp "NOTB" x
         | rexp(T.ANDB x) = binOp "ANDB" x
         | rexp(T.ORB x) = binOp "ORB" x
         | rexp(T.XORB x) = binOp "XORB" x
         | rexp(T.EQVB x) = binOp "EQVB" x
         | rexp(T.SLL x) = binOp "SLL" x
         | rexp(T.SRL x) = binOp "SRL" x
         | rexp(T.SRA x) = binOp "SRA" x
         | rexp(T.SX(t1,t2,x)) = app("SX",[int t1,int t2,rexp x])
         | rexp(T.ZX(t1,t2,x)) = app("ZX",[int t1,int t2,rexp x])
         | rexp(T.CVTF2I(t1,r,t2,x)) =
               app("CVTF2I",[int t1,id(T.Basis.roundingModeToString r),
                             int t2,fexp x])
         | rexp(T.COND(ty,cc,a,b)) =
              app("COND",[int ty,ccexp cc,rexp a,rexp b])
         | rexp(T.$(ty,k,e)) = app("$",[int ty,cellkind k,rexp e])
         | rexp(T.ARG(ty,a,b)) = arg(ty,a,b)
         | rexp(T.PARAM(i)) = app("PARAM",[int i])
         | rexp(T.???) = id "???"
         | rexp(T.OP(ty,opc,es)) =
              app("OP",[int ty,oper opc,list(map rexp es, NONE)])
         | rexp(T.BITSLICE(ty,sl,e)) =
              app("BITSLICE",[int ty,slice sl,rexp e])
         | rexp e = error("transRTL: "^RTL.Util.rexpToString e)
       and slice sl = list(map (fn (x,y) => tuple[int x,int y]) sl, NONE)
       and fbinOp n (ty,x,y) = app(n,[int ty,fexp x,fexp y])
       and funaryOp n (ty,x) = app(n,[int ty,fexp x])
       and fexp(T.FADD x) = fbinOp "FADD" x
         | fexp(T.FSUB x) = fbinOp "FSUB" x
         | fexp(T.FMUL x) = fbinOp "FMUL" x
         | fexp(T.FDIV x) = fbinOp "FDIV" x
         | fexp(T.FCOPYSIGN x) = fbinOp "FCOPYSIGN" x
         | fexp(T.FNEG x) = funaryOp "FNEG" x
         | fexp(T.FABS x) = funaryOp "FABS" x
         | fexp(T.FSQRT x) = funaryOp "FSQRT" x
         | fexp(T.FCOND(ty,cc,x,y)) =
              app("FCOND",[int ty,ccexp cc,fexp x,fexp y])
         | fexp(T.CVTI2F(t1,t2,x)) = app("CVTI2F",[int t1,int t2,rexp x])
         | fexp(T.CVTF2F(t1,t2,x)) = app("CVTF2F",[int t1,int t2,fexp x])
         | fexp e = error("transRTL: "^RTL.Util.fexpToString e)

       and stm(T.ASSIGN(ty,x,y)) = app("ASSIGN",[int ty,rexp x,rexp y])
         | stm(T.JMP(e,_)) = app("JMP",[rexp e,Nil])
         | stm(T.RET _) = app("RET",[Nil])
         | stm(T.IF(x,y,z)) = app("IF",[ccexp x,stm y,stm z])
         | stm(T.SEQ ss) = app("SEQ",[list(map stm ss,NONE)])
         | stm(T.RTL{e, ...}) = stm e
         | stm(T.CALL{funct,...}) = app("CALL",
                [record[("defs",Nil),
                        ("uses",Nil),
                        ("funct",rexp funct),
                        ("targets",Nil),
                        ("region",region)]
                ]
            )
         | stm s = error("transRTL: "^RTL.Util.stmToString s)

       and ccexp(T.CMP(ty,cc,x,y)) =
             app("CMP",[int ty,id(T.Basis.condToString cc),rexp x,rexp y])
         | ccexp(T.FCMP(ty,cc,x,y)) =
             app("FCMP",[int ty,id(T.Basis.fcondToString cc),fexp x,fexp y])
         | ccexp(T.TRUE)     = id "TRUE"
         | ccexp(T.FALSE)    = id "FALSE"
         | ccexp(T.AND(x,y)) = app("AND",[ccexp x,ccexp y])
         | ccexp(T.OR(x,y))  = app("OR",[ccexp x,ccexp y])
         | ccexp(T.XOR(x,y)) = app("XOR",[ccexp x,ccexp y])
         | ccexp(T.EQV(x,y)) = app("EQV",[ccexp x,ccexp y])
         | ccexp(T.NOT x)    = app("NOT",[ccexp x])
         | ccexp e = error("transRTL: "^RTL.Util.ccexpToString e)
   in  stm rtl
   end

   (*========================================================================
    * Translate an RTL to an expression
    *========================================================================*)
   fun rtlToExp rtl =
   let fun id name = A.IDexp(A.IDENT(["T"],name))
       fun app(n, es) = A.APPexp(id n, A.TUPLEexp es)
       val int = U.INTexp
       val string= U.STRINGexp
       fun arg(ty,a,name) = A.IDexp(A.IDENT([],name))
       fun cellkind k = A.IDexp(A.IDENT(["C"],CellsBasis.cellkindToString k))
       fun oper(T.OPER{name,...}) = A.IDexp(A.IDENT(["P"],name))
       val region=A.IDexp(A.IDENT(["T","Region"],"memory"))
   in  transRTL{id=id, app=app, list=A.LISTexp, string=string,
                int=int, intinf=U.INTINFexp,
                word32=U.WORD32exp, Nil=A.LISTexp([],NONE),
                tuple=A.TUPLEexp, record=A.RECORDexp,
                region=region, arg=arg, cellkind=cellkind, oper=oper
               } rtl
   end

   (*========================================================================
    * Translate an RTL to a pattern
    *========================================================================*)
   fun rtlToPat rtl =
   let fun mkId name = A.IDENT(["T"],name)
       fun id name = A.CONSpat(mkId name,NONE)
       fun app(n, [x]) = A.CONSpat(mkId n, SOME x)
         | app(n, es) = A.CONSpat(mkId n, SOME(A.TUPLEpat es))
       fun record ps = A.RECORDpat(ps,false)
       val int = U.INTpat
       val intinf= U.INTINFpat
       val string= U.STRINGpat
       fun arg(ty,a,name) = A.IDpat name
       fun cellkind k = A.IDpat(CellsBasis.cellkindToString k)
       fun oper(T.OPER{name,...}) =
          A.CONSpat(A.IDENT(["T"],"OPER"),
            SOME(A.RECORDpat([("name",U.STRINGpat name)],true)))
       val region=A.WILDpat
   in  transRTL{id=id, app=app, list=A.LISTpat, string=string,
                int=int, intinf=intinf,
                word32=U.WORD32pat, Nil=A.LISTpat([],NONE),
                tuple=A.TUPLEpat, record=record, region=region,
                arg=arg, cellkind=cellkind, oper=oper
               } rtl
   end

   (*========================================================================
    * Translate an RTL to a function with arguments
    *========================================================================*)
   fun rtlToFun(rtlName, rtlArgs, rtl) =
   let val body = rtlToExp rtl
       val args = A.RECORDpat(map (fn id => (id,A.IDpat id)) rtlArgs,false)
   in  A.FUNdecl
          [A.FUNbind(rtlName, [A.CLAUSE([args], NONE, body)])]
   end

   (*========================================================================
    * Create a new_op
    *========================================================================*)
   fun createNewOp{name, hash, attribs} =
       A.VALdecl[
         A.VALbind(A.IDpat name,
            A.APPexp(A.IDexp(A.IDENT(["T"],"OPER")),
               A.APPexp(A.IDexp(A.IDENT(["RTL"],"newOp")),
                  A.RECORDexp[("name",U.STRINGexp name),
                              ("attribs",U.WORDexp (!attribs))
                             ])))
       ]

end

