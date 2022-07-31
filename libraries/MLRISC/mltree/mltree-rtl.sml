(*
 * Basic RTLs and query functions on these RTLs
 *
 * -- Allen
 *)
functor MLTreeRTL
   (structure Util    : MLTREE_UTILS 
    structure Rewrite : MLTREE_REWRITE
    structure Fold    : MLTREE_FOLD
       sharing Util.T = Rewrite.T = Fold.T
   ) : MLTREE_RTL =
struct
 
   structure T       = Util.T
   structure Util    = Util
   structure Rewrite = Rewrite
   structure Fold    = Fold
   structure W       = Word
   structure C       = CellsBasis

   fun error msg = MLRiscErrorMsg.error("MLTreeRTL",msg)

   datatype pos = IN of int | OUT of int | IO of int * int
   datatype arity = ZERO | ONE | MANY

   val itow = Word.fromInt
   infix ||
   val op || = W.orb 
 
   type ty       = T.ty
   type rtl      = T.stm
   type exp      = T.rexp
   type cond     = T.ccexp
   type var      = T.var
   type hasher   = T.hasher
   type equality = T.equality
   type printer  = T.printer
   type div_rounding_mode = T.div_rounding_mode

   val hashRTL     = Util.hashStm
   val eqRTL       = Util.eqStm
   val showRTL     = Util.show
   val rtlToString = Util.stmToString
   val expToString = Util.rexpToString

   (*-----------------------------------------------------------------------
    * Attributes
    *-----------------------------------------------------------------------*)
   val A_TRAPPING   = W.<<(0w1,0w1)  (* may cause traps *)
   val A_PINNED     = W.<<(0w1,0w2)  (* cannot be moved *)
   val A_SIDEEFFECT = W.<<(0w1,0w3)  (* has side effect *)
   val A_MUTATOR    = W.<<(0w1,0w4)
   val A_LOOKER     = W.<<(0w1,0w5)
   val A_BRANCH     = W.<<(0w1,0w6)  (* conditional branch *)
   val A_JUMP       = W.<<(0w1,0w7)  (* has control flow *)
   val A_PURE       = 0wx0
   fun isOn(a,flag) = Word.andb(a,flag) <> 0w0

   (*-----------------------------------------------------------------------
    * Create new RTL operators 
    *-----------------------------------------------------------------------*)
   val hashCnt   = ref 0w0
   fun newHash() = let val h = !hashCnt in hashCnt := h + 0w124127; h end
   fun newOp{name,attribs} = {name=name,attribs=ref attribs,hash=newHash()}

   (*-----------------------------------------------------------------------
    *  Reduce a RTL to compiled internal form
    *-----------------------------------------------------------------------*)
   fun reduce rtl =
   let 
   in  rtl
   end

   (*-----------------------------------------------------------------------
    * Collect attributes
    *-----------------------------------------------------------------------*)
   fun attribsOf rtl = 
   let fun stm(T.STORE _,a)     = a || (A_SIDEEFFECT || A_MUTATOR)
         | stm(T.JMP _, a)      = a || (A_JUMP || A_SIDEEFFECT)
         | stm(T.IF _, a)       = a || (A_BRANCH || A_JUMP || A_SIDEEFFECT)
         | stm(T.RET _, a)      = a || (A_JUMP || A_SIDEEFFECT)
         | stm(T.CALL _, a)     = a || A_SIDEEFFECT
         | stm(T.ASSIGN(_,T.$(_,C.MEM,addr),value),a) =
               a || (A_SIDEEFFECT || A_MUTATOR)
         | stm(_, a) = a
       fun rexp(T.ADDT _,a) = a || A_TRAPPING
         | rexp(T.SUBT _,a) = a || A_TRAPPING
         | rexp(T.MULT _,a) = a || A_TRAPPING
         | rexp(T.DIVT _,a) = a || A_TRAPPING
         | rexp(T.LOAD _,a) = a || A_LOOKER
         | rexp(T.$(_,C.MEM,_),a) = a || A_LOOKER
         | rexp(_, a) = a
       fun fexp(_, a) = a
       fun ccexp(_, a) = a
   in  #stm (Fold.fold{stm=stm,rexp=rexp, fexp=fexp, ccexp=ccexp}) rtl
   end


   (*-----------------------------------------------------------------------
    * Create a uniq RTL 
    *-----------------------------------------------------------------------*)
   fun new(rtl) = 
   let val rtl = reduce rtl
       val attribs = attribsOf(rtl, A_PURE)
       val rtl = 
         case rtl of
           T.COPY _ => rtl
         | _ => T.RTL{e=rtl,hash=newHash(),attribs=ref attribs}
   in  rtl 
   end

   val COPY = T.COPY(0,[],[])
   val JMP  = new(T.JMP(T.PARAM 0,[]))


   fun pin(x as T.RTL{attribs, ...}) = 
        (attribs := (!attribs || A_PINNED); x)
     | pin _ = error "pin"

   (*-----------------------------------------------------------------------
    * Type queries
    *-----------------------------------------------------------------------*)
   fun hasSideEffect(T.RTL{attribs, ...}) = isOn(!attribs, A_SIDEEFFECT)
     | hasSideEffect _ = false
   fun isConditionalBranch(T.RTL{attribs, ...}) = isOn(!attribs,A_BRANCH)
     | isConditionalBranch _ = false
   fun isJump(T.RTL{attribs, ...}) = isOn(!attribs,A_JUMP)
     | isJump(T.JMP _) = true
     | isJump _ = false
   fun isLooker(T.RTL{attribs, ...}) = isOn(!attribs,A_LOOKER)
     | isLooker _ = false

   (*-----------------------------------------------------------------------
    * Def/use queries
    *-----------------------------------------------------------------------*)
   fun defUse rtl = 
   let fun contains x = List.exists(fn y => Util.eqRexp(x,y))
       fun diff(A,B) = List.filter (fn z => not(contains z B)) A
       fun uniq([], l) = rev l
         | uniq(x::xs, l) = if contains x l then uniq(xs,l) else uniq(xs,x::l)

       fun stm(T.ASSIGN(_,x, y), d, u) = 
           let val (d, u) = lhs(x, d, u)
           in  rhs(y, d, u) end
         | stm(T.COPY _, d, u) = (d, u) (* XXX *)
         | stm(T.RET _, d, u) = (d, u)
         | stm(T.RTL{e, ...}, d, u) = stm(e, d, u) 
         | stm(T.JMP(e,_), d, u) = rhs(e, d, u)
         | stm(T.IF(x,y,z), d, u) = 
             let val (d, u)  = cond(x, d, u)
                 val (d1, u) = stm(y, [], u)
                 val (d2, u) = stm(z, [], u)
                 val u1      = diff(d1,d2)
                 val u2      = diff(d2,d1)
             in  (d @ d1 @ d2, u @ u1 @ u2)
             end
         | stm(T.SEQ rtls, d, u) = stms(rtls, d, u)
         | stm(T.CALL{funct,...}, d, u) = rhs(funct, d, u)
         | stm(rtl, d, u) = error("defUse.stm: "^rtlToString rtl)

       and stms([], d, u) = (d, u)
         | stms(s::ss, d, u) = let val (d, u) = stm(s, d, u)
                               in  stms(ss, d, u) end

       and rhs(T.LI _, d, u) = (d, u)
         | rhs(x as T.ARG _, d, u) = (d, x::u)
         | rhs(x as T.PARAM _, d, u) = (d, x::u)
         | rhs(T.ADD(_,x,y), d, u) = binOp(x, y, d, u)
         | rhs(T.SUB(_,x,y), d, u) = binOp(x, y, d, u)
         | rhs(T.MULS(_,x,y), d, u) = binOp(x, y, d, u)
         | rhs(T.MULU(_,x,y), d, u) = binOp(x, y, d, u)
         | rhs(T.DIVS(_,_,x,y), d, u) = binOp(x, y, d, u)
         | rhs(T.DIVU(_,x,y), d, u) = binOp(x, y, d, u)
         | rhs(T.REMS(_,_,x,y), d, u) = binOp(x, y, d, u)
         | rhs(T.REMU(_,x,y), d, u) = binOp(x, y, d, u)
         | rhs(T.ADDT(_,x,y), d, u) = binOp(x, y, d, u)
         | rhs(T.SUBT(_,x,y), d, u) = binOp(x, y, d, u)
         | rhs(T.MULT(_,x,y), d, u) = binOp(x, y, d, u)
         | rhs(T.DIVT(_,_,x,y), d, u) = binOp(x, y, d, u)
         | rhs(T.SLL(_,x,y), d, u) = binOp(x, y, d, u)
         | rhs(T.SRL(_,x,y), d, u) = binOp(x, y, d, u)
         | rhs(T.SRA(_,x,y), d, u) = binOp(x, y, d, u)
         | rhs(T.ANDB(_,x,y), d, u) = binOp(x, y, d, u)
         | rhs(T.ORB(_,x,y), d, u) = binOp(x, y, d, u)
         | rhs(T.XORB(_,x,y), d, u) = binOp(x, y, d, u)
         | rhs(T.EQVB(_,x,y), d, u) = binOp(x, y, d, u)
         | rhs(T.NEG(_,x), d, u) = rhs(x, d, u)
         | rhs(T.NEGT(_,x), d, u) = rhs(x, d, u)
         | rhs(T.NOTB(_,x), d, u) = rhs(x, d, u)
         | rhs(T.SX(_,_,x), d, u) = rhs(x, d, u)
         | rhs(T.ZX(_,_,x), d, u) = rhs(x, d, u)
         | rhs(x as T.$(_,_,T.ARG _), d, u) = (d, x::u)
         | rhs(x as T.$(_,_,T.PARAM _), d, u) = (d, x::u)
         | rhs(x as T.$(_,_,e), d, u) = rhs(e, d, x::u)
         | rhs(T.CVTF2I(_,_,_,x), d, u) = fexp(x, d, u)
         | rhs(T.OP(_,_,es), d, u) = rexps(es, d, u)
         | rhs(T.COND(_,x,y,z), d, u) = 
            let val (d, u) = cond(x, d, u)
            in  binOp(y, z, d, u) end
         | rhs(T.BITSLICE(_,_,e), d, u) = rhs(e, d, u)
         | rhs(T.???, d, u) = (d, u)
         | rhs(e, d, u) = error("defUse.rhs: "^Util.rexpToString e)

       and binOp(x, y, d, u) =
           let val (d, u) = rhs(x, d, u)
           in  rhs(y, d, u) end

       and rexps([], d, u) = (d, u)
         | rexps(e::es, d, u) =
           let val (d, u) = rhs(e, d, u)
           in  rexps(es, d, u) end

       and lhs(x as T.$(_,_,T.ARG _), d, u) = (x::d, u)
         | lhs(x as T.$(_,_,T.PARAM _), d, u) = (x::d, u)
         | lhs(x as T.$(_,_,addr), d, u) = rhs(addr, x::d, u)
         | lhs(x as T.ARG _, d, u) = (x::d, u)
         | lhs(x as T.PARAM _, d, u) = (x::d, u)
         | lhs(T.???, d, u) = (d, u)
         | lhs(e, d, u) = error("defUse.lhs: "^Util.rexpToString e)

       and fexp(T.FADD(_, x, y), d, u) = fbinOp(x, y, d, u)
         | fexp(T.FSUB(_, x, y), d, u) = fbinOp(x, y, d, u)
         | fexp(T.FMUL(_, x, y), d, u) = fbinOp(x, y, d, u)
         | fexp(T.FDIV(_, x, y), d, u) = fbinOp(x, y, d, u)
         | fexp(T.FCOPYSIGN(_, x, y), d, u) = fbinOp(x, y, d, u)
         | fexp(T.FCOND(_, x, y, z), d, u) = 
           let val (d, u) = cond(x, d, u)
           in  fbinOp(y, z, d, u) end
         | fexp(T.FSQRT(_, x), d, u) = fexp(x, d, u)
         | fexp(T.FABS(_, x), d, u) = fexp(x, d, u)
         | fexp(T.FNEG(_, x), d, u) = fexp(x, d, u)
         | fexp(T.CVTI2F(_, _, x), d, u) = rhs(x, d, u)
         | fexp(e, d, u) = error("defUse.fexp: "^Util.fexpToString e)

       and fbinOp(x, y, d, u) =
           let val (d, u) = fexp(x, d, u)
           in  fexp(y, d, u) end

       and cond(T.CMP(_,_,x,y), d, u) = binOp(x, y, d, u)
         | cond(T.FCMP(_,_,x,y), d, u) = fbinOp(x, y, d, u)
         | cond(T.TRUE, d, u) = (d, u)
         | cond(T.FALSE, d, u) = (d, u)
         | cond(T.NOT x, d, u) = cond(x, d, u)
         | cond(T.AND(x, y), d, u) = cond2(x, y, d, u)
         | cond(T.OR(x, y), d, u) = cond2(x, y, d, u)
         | cond(T.XOR(x, y), d, u) = cond2(x, y, d, u)
         | cond(T.EQV(x, y), d, u) = cond2(x, y, d, u)
         | cond(e, d, u) = error("defUse.cond: "^Util.ccexpToString e)

       and cond2(x, y, d, u) =
           let val (d, u) = cond(x, d, u)
           in  cond(y, d, u) end

       val (d, u) = stm(rtl, [], [])

   in  (uniq(d, []), uniq(u, []))
   end

   (*-----------------------------------------------------------------------
    * Giving definitions and uses.  Find out the naming constraints. 
    *-----------------------------------------------------------------------*)
   fun namingConstraints(defs, uses) =
   let fun collectFixed((x as T.$(_,_,T.LI r))::xs, fixed, rest) =
              collectFixed(xs, (x, IntInf.toInt r)::fixed, rest)
         | collectFixed(x::xs, fixed, rest) = 
              collectFixed(xs, fixed, x::rest)
         | collectFixed([], fixed, rest) = (fixed, rest)
       val (fixedUses, otherUses) = collectFixed(uses, [], [])
       val (fixedDefs, otherDefs) = collectFixed(defs, [], [])
       val fixed = 
          List.filter 
             (fn x => List.exists (fn y => Util.eqRexp(x,y)) otherUses)
                      otherDefs
   in  {fixedUses=fixedUses,
        fixedDefs=fixedDefs,
        twoAddress=fixed
       }
   end

   (*-----------------------------------------------------------------------
    * Assign positions to each argument
    *-----------------------------------------------------------------------*)
   fun argPos rtl =
   let val (defs, uses) = defUse rtl
       fun pos([], i, ds) = ds
         | pos(d::defs, i, ds) = pos(defs, i+1, (d,i)::ds)
       val ds = pos(defs, 0, [])
       val us = pos(uses, 0, [])
   in  (ds, us)
   end

   exception NotAnArgument

   fun argOf rtl =
   let val (defs, uses) = argPos rtl
       fun find(this,(x as (T.$(_,_,T.ARG(_,_,name)),_))::xs) =
            if this = name then SOME x else find(this, xs)
         | find(this,(x as (T.ARG(_,_,name),_))::xs) =
            if this = name then SOME x else find(this, xs)
         | find(this,_::xs) = find(this, xs)
         | find(this,[]) = NONE
       fun lookup name = 
         case (find(name,defs), find(name,uses)) of
           (SOME(x,i),SOME(_,j)) => (x,IO(i,j))
         | (SOME(x, i), NONE) => (x,OUT i)
         | (NONE, SOME(x, i)) => (x,IN i)
         | (NONE, NONE) => raise NotAnArgument
   in  lookup 
   end
 
   (*-----------------------------------------------------------------------
    * Return the arity of an argument
    *-----------------------------------------------------------------------*)
   fun arity(T.ARG _) = MANY
     | arity(T.$(_,C.MEM,_)) = MANY
     | arity(T.$(_,_,_)) = ONE
     | arity _ = raise NotAnArgument

   fun nonConstArity(T.ARG _) = MANY
     | nonConstArity(T.$(_,C.MEM,_)) = MANY
     | nonConstArity(T.$(_,_,_)) = ONE
     | nonConstArity _        = raise NotAnArgument

   (*-----------------------------------------------------------------------
    * Code motion queries
    *-----------------------------------------------------------------------*)
   fun can'tMoveUp(T.RTL{attribs, ...}) = 
          isOn(!attribs, A_SIDEEFFECT || A_TRAPPING || A_PINNED)
     | can'tMoveUp(T.PHI _) = true
     | can'tMoveUp(T.SOURCE) = true
     | can'tMoveUp(T.SINK) = true
     | can'tMoveUp _ = false

   fun can'tMoveDown(T.PHI _) = true
     | can'tMoveDown(T.SOURCE) = true
     | can'tMoveDown(T.SINK) = true
     | can'tMoveDown(T.RTL{attribs, ...}) = 
          isOn(!attribs, A_SIDEEFFECT || A_BRANCH || A_JUMP || A_TRAPPING ||
                        A_PINNED ||
                        A_LOOKER (* can be avoided with pure loads! XXX *))
     | can'tMoveDown rtl = error("can'tMoveDown: "^rtlToString rtl)

   fun pinned(T.RTL{attribs, ...}) = 
         isOn(!attribs, A_SIDEEFFECT || A_TRAPPING || A_PINNED)
     | pinned(T.PHI _) = true
     | pinned(T.SOURCE) = true
     | pinned(T.SINK) = true
     | pinned _ = false
   fun can'tBeRemoved(T.RTL{attribs, ...}) = 
         isOn(!attribs, A_SIDEEFFECT || A_BRANCH || A_JUMP)
     | can'tBeRemoved(T.SOURCE) = true
     | can'tBeRemoved(T.SINK) = true
     | can'tBeRemoved _ = false
 
end
