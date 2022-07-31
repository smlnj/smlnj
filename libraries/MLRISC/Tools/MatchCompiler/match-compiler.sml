(*
 * A pattern matching compiler. 
 * This is based on Pettersson's paper
 * ``A Term Pattern-Match Compiler Inspired by Finite Automata Theory''
 *
 *)
local
   val sanityCheck = true
   val debug       = false
in

functor MatchCompiler
   (structure Var  : (* a variable *)
    sig type var 
        val compare : var * var -> order 
        val toString : var -> string
    end

    structure Con : (* datatype constructors *)
    sig
       type con 
       val compare     : con * con -> order
       val toString    : con -> string
       val variants    : con -> {known:con list, others:bool}
       val arity       : con -> int
    end  

    structure Literal : (* literals *)
    sig
       type literal
       val compare  : literal * literal -> order
       val toString : literal -> string
       val variants : literal -> {known:literal list, others:bool} option
    end

    structure Action :   
    sig type action  (* an action *)
        val toString : action -> string
        val freeVars : action -> Var.var list
    end

    structure Guard  : (* a guard expression *)
    sig type guard 
        val toString   : guard -> string
        val compare    : guard * guard -> order
        val logicalAnd : guard * guard -> guard
    end

    structure Exp :
    sig type exp
        val toString : exp -> string
    end
   ) : MATCH_COMPILER =
struct

   structure PP = PP

   val i2s = Int.toString

   fun listify (l,s,r) list = 
       l^List.foldr (fn (x,"") => x | (x,y) => x^s^y) "" list^r

   (* ListPair.all has the wrong semantics! *)
   fun forall f ([], []) = true
     | forall f (x::xs, y::ys) = f(x,y) andalso forall f (xs, ys)
     | forall f _ = false

   datatype index = INT of int | LABEL of Var.var

   datatype path  = PATH of index list

   structure Index =
   struct
      fun compare(INT i, INT j) = Int.compare(i,j)
        | compare(LABEL i, LABEL j) = Var.compare(i,j)
        | compare(INT _, LABEL _) = LESS
        | compare(LABEL _,INT _) = GREATER
      fun equal(x,y) = compare(x,y) = EQUAL
      fun toString(INT i) = i2s i
        | toString(LABEL l) = Var.toString l
   end

   structure Path =
   struct
      fun compare(PATH p1, PATH p2) =
      let fun loop([], []) = EQUAL
            | loop([], _)  = LESS
            | loop(_, [])  = GREATER
            | loop(x::xs, y::ys) =
              (case Index.compare(x,y) of
                EQUAL => loop(xs,ys)
              | ord   => ord
              )
      in  loop(p1, p2) 
      end
      fun equal(p1,p2) = compare(p1,p2) = EQUAL
      fun append(PATH p1, PATH p2) = PATH(p1@p2)
      fun dot(PATH p, i) = PATH(p @ [i])
      fun toString(PATH p) =
          "["^List.foldr (fn (i,"") => Index.toString i
                           | (i,s) => Index.toString i^"."^s) "" p^"]"
      fun toIdent(PATH p) = 
          "v_"^List.foldr (fn (i,"") => Index.toString i
                            | (i,s) => Index.toString i^"_"^s) "" p
      structure Map = RedBlackMapFn(type ord_key = path val compare = compare)
   end

   datatype name = VAR of Var.var | PVAR of path

   structure Name =
   struct
      fun toString(VAR v)  = Var.toString v
        | toString(PVAR p) = Path.toString p 
      fun compare(VAR x,VAR y) = Var.compare(x,y) 
        | compare(PVAR x,PVAR y) = Path.compare(x,y) 
        | compare(VAR  _, PVAR _) = LESS
        | compare(PVAR  _, VAR _) = GREATER
      fun equal(x,y) = compare(x,y) = EQUAL
      structure Set = RedBlackSetFn(type ord_key = name val compare = compare)
      fun setToString s = 
          "{"^List.foldr (fn (v,"") =>toString v
                           | (v,s) => toString v^"."^s) "" (Set.listItems s)^"}"
   end

   structure VarSet = RedBlackSetFn
      (type ord_key = Var.var val compare = Var.compare)
   structure Subst = RedBlackMapFn(type ord_key = Var.var val compare = Var.compare)
   type subst = name Subst.map
   fun mergeSubst(s1,s2) = Subst.foldri(fn (k,v,s) => Subst.insert(s,k,v)) s1 s2

   (* Internal rep of pattern after every variable has been renamed *) 
   datatype pat = 
     WILDpat                               (* wild card *)
   | APPpat of decon * pat list            (* constructor *)
   | TUPLEpat of pat list                  (* tupling *)
   | RECORDpat of (Var.var * pat) list     (* record *)
   | ORpat of (subst * pat) list           (* disjunction *)
   | ANDpat of (subst * pat) list          (* conjunction *)  
   | NOTpat of subst * pat                 (* negation *)
   | WHEREpat of pat * subst * Guard.guard   (* guard *)
   | NESTEDpat of pat * subst * path * (int * Exp.exp) * pat
   | CONTpat of Var.var * pat 

   and decon = CON of Con.con          
             | LIT of Literal.literal   

   exception MatchCompiler of string

   fun error msg = raise MatchCompiler msg 
   fun bug msg   = error("bug: "^msg)

   structure Con     = Con
   structure Action  = Action
   structure Literal = Literal
   structure Guard   = Guard
   structure Exp     = Exp
   structure Var     = Var

   structure Decon =
   struct
      fun kind(CON _) = 0
        | kind(LIT _) = 1
      fun compare(CON x,CON y) = Con.compare(x,y)
        | compare(LIT x,LIT y) = Literal.compare(x,y)
        | compare(x,y) = Int.compare(kind x,kind y)

      fun toString(CON c) = Con.toString c
        | toString(LIT l) = Literal.toString l

      fun equal(x,y) = compare(x,y) = EQUAL
      structure Map = RedBlackMapFn(type ord_key = decon val compare = compare)
      structure Set = RedBlackSetFn(type ord_key = decon val compare = compare)
   end 

   structure Pat =
   struct
      fun sortByLabel l =
          ListMergeSort.sort 
            (fn ((x,_),(y,_)) => Var.compare(x,y) = GREATER) l

      fun toString(WILDpat) = "_"
        | toString(APPpat(c,[])) = Decon.toString c
        | toString(APPpat(c,xs)) = Decon.toString c^
                                 listify("(",",",")") (map toString xs)
        | toString(TUPLEpat pats) = listify("(",",",")") (map toString pats)
        | toString(RECORDpat lps) = listify("{",",","}") 
                                 (map (fn (l,p) =>
                                      Var.toString l^"="^toString p) lps)
        | toString(ORpat ps) = listify("("," | ",")") (map toString' ps)
        | toString(ANDpat ps) = listify("("," and ",")") (map toString' ps)
        | toString(NOTpat p)  = "not "^toString' p
        | toString(WHEREpat(p,_,g)) = toString p^" where "^Guard.toString g
        | toString(NESTEDpat(p,_,_,(_,e),p')) =
                toString p^" where "^Exp.toString e^" in "^toString p' 
        | toString(CONTpat(v,p)) = toString p ^" exception "^ Var.toString v
      and toString'(subst,p) = toString p

   end

   type rule_no = int

   datatype dfa = 
       DFA of  
       { stamp    : int,              (* unique dfa stamp *)
         freeVars : Name.Set.set ref, (* free variables *)
         refCount : int ref,          (* reference count *)
         generated: bool ref,         (* has code been generated? *)
         height   : int ref,          (* dag height *)
         test     : test              (* type of tests *)
       }

   and test = 
         CASE   of path * (decon * path list * dfa) list * 
                   dfa option (* multiway *)
       | WHERE  of Guard.guard * dfa * dfa            (* if test *)
       | OK     of rule_no * Action.action            (* final dfa *)
       | BIND   of subst * dfa                        (* apply subst *)
       | LET    of path * (int * Exp.exp) * dfa       (* let *)
       | SELECT of path * (path * index) list * dfa   (* projections *)
       | CONT   of Var.var * dfa                      (* bind continuation *)
       | FAIL                                         (* error dfa *)

   and compiled_dfa  = 
          ROOT of {dfa        : dfa, 
                   used       : Name.Set.set,
                   exhaustive : bool,
                   redundant  : IntListSet.set
                  }

   and matrix = 
       MATRIX of
       { rows  : row list,
         paths : path list                       (* path (per column) *)
       }
       

   withtype row =  
              {pats   : pat list, 
               guard  : (subst * Guard.guard) option,
               nested : (subst * path * (int * Exp.exp) * pat) list,
               dfa    : dfa
              } 
       and compiled_rule = 
             rule_no * pat list * Guard.guard option * subst * Action.action

       and compiled_pat = pat * subst

   (* Utilities for dfas *)
   structure DFA =
   struct
      val itow = Word.fromInt 

      fun h(DFA{stamp, ...}) = itow stamp
      fun hash(DFA{stamp, test, ...}) = 
          (case test of
            FAIL    => 0w0
          | OK _    => 0w123 + itow stamp
          | CASE(path, cases, default) => 0w1234 +
               foldr (fn ((_,_,x),y) => h x + y) 
                     (case default of SOME x => h x | NONE => 0w0) cases
          | SELECT(_, _, dfa) => 0w2313 + hash dfa
          | CONT(_, dfa) => 0w1234 + hash dfa
          | WHERE(g, yes, no) => 0w2343 + h yes + h no
          | BIND(_, dfa) => 0w23234 + h dfa
          | LET(_, (i, _), dfa) => itow i + h dfa + 0w843
          )

      (* pointer equality *)
      fun eq(DFA{stamp=s1, ...}, DFA{stamp=s2, ...}) = s1=s2
      fun eqOpt(NONE, NONE) = true
        | eqOpt(SOME x, SOME y) = eq(x,y)
        | eqOpt _ = false

      (* one-level equality *)
      fun equal(DFA{test=t1, stamp=s1,...},
                DFA{test=t2, stamp=s2,...}) =
             (case (t1, t2) of
                (FAIL, FAIL) => true
              | (OK _, OK _) => s1 = s2
              | (SELECT(p1, b1, x), SELECT(p2, b2, y)) => 
                 Path.equal(p1,p2) andalso eq(x,y) andalso
                 forall(fn ((px,ix),(py,iy)) =>
                    Path.equal(px,py) andalso Index.equal(ix,iy))
                     (b1,b2)
              | (CONT(k1, x), CONT(k2, y)) => 
                 Var.compare(k1,k2) = EQUAL andalso eq(x,y)
              | (CASE(p1,c1,o1), CASE(p2,c2,o2)) =>
                  Path.equal(p1,p2) andalso 
                  forall
                     (fn ((u,_,x),(v,_,y)) => 
                          Decon.equal(u,v) andalso eq(x,y)) 
                        (c1,c2) andalso
                  eqOpt(o1,o2)
              | (WHERE(g1, y1, n1), 
                 WHERE(g2, y2, n2)) =>
                  Guard.compare(g1,g2) = EQUAL 
                  andalso eq(y1,y2) andalso eq(n1,n2) 
              | (BIND(s1, x), BIND(s2, y)) =>
                  eq(x,y) andalso
                    forall (fn ((p,x),(q,y)) =>
                             Var.compare(p,q) = EQUAL andalso 
                             Name.equal(x,y))
                      (Subst.listItemsi s1, Subst.listItemsi s2)
              | (LET(p1, (i1, _), x), LET(p2, (i2, _), y)) =>
                  Path.equal(p1,p2) andalso i1=i2 andalso eq(x,y)
              | _ => false
             )

      structure HashTable = 
         HashTableFn(type hash_key = dfa
                     val sameKey = equal
                     val hashVal = hash
                    )

      fun toString(ROOT{dfa, ...}) =
      let exception NotVisited
          val visited = IntHashTable.mkTable(32, NotVisited)
          fun mark stamp = IntHashTable.insert visited (stamp, true)
          fun isVisited stamp = 
              Option.getOpt(IntHashTable.find visited stamp, false)
          open PP
          infix ++
          fun prArgs [] = nop
            | prArgs ps = seq(!!"(",!!",",!!")") (map (! o Path.toString) ps)
          fun walk(DFA{stamp, test=FAIL, ...}) = ! "fail"
            | walk(DFA{stamp, test, refCount=ref n, ...}) =
              if isVisited stamp then !"goto" ++ int stamp 
              else (mark stamp;
                    !!"<" ++ int stamp ++ !!">" ++
                    (if n > 1 then !! "*" else nop) ++
                    (case test of
                      OK(_,a) => !"Ok" ++ !(Action.toString a)
                    | FAIL => !"Fail"
                    | SELECT(root,bindings,body) => 
                      line(!"Let") ++
                      block(seq (nop,nl,nop) 
                              (map (fn (p,i) =>
                               tab ++
                               !(Path.toString p) ++ !"=" ++ 
                               !(Path.toString root) ++ !"." ++ 
                                 !(Index.toString i)
                                ) bindings) 
                           ) ++
                      line(!"in") ++
                      block(walk body)
                    | CONT(k,x) => line(!"Cont" ++ !(Var.toString k) ++ walk x)
                    | CASE(p,cases,default) =>
                      line(!"Case" ++ !!(Path.toString p)) ++
                       block(
                          seq (nop,nl,nop) 
                           ((map (fn (decon,args,dfa) =>
                             tab ++ !(Decon.toString decon) ++ prArgs args
                                 ++ !"=>" ++ sp ++ walk dfa)
                               cases) @
                             (case default of
                               NONE => []
                             | SOME dfa => [!"_" ++ !"=>" ++ sp ++ walk dfa]
                             )
                          )
                       )
                    | WHERE(g,y,n) =>
                      line(!"If" ++ !(Guard.toString g)) ++
                      block(tab ++ ! "then" ++ walk y ++ nl ++
                            tab ++ ! "else" ++ walk n)
                    | BIND(subst, x) =>
                      line(Subst.foldri (fn (v,n,pp) =>
                           tab ++ !(Var.toString v) ++ !!"<-" ++
                                  !(Name.toString n) ++ pp)
                               nop subst) ++
                           walk x
                    | LET(path,( _, e), x) =>
                      line(! "Let" ++ !(Path.toString path) ++ !"=" ++ 
                           !(Exp.toString e)) ++
                      block(walk x) 
                    )
                   )
      in  PP.text(walk dfa ++ nl)
      end
   end

   (* Utilities for the pattern matrix *)
   structure Matrix =
   struct
       fun row(MATRIX{rows, ...}, i) = List.nth(rows,i)
       fun col(MATRIX{rows, ...}, i) = 
             List.map (fn {pats, ...} => List.nth(pats, i)) rows
       fun pathOf(MATRIX{paths, ...}, i) = List.nth(paths, i)
       fun columnCount(m) = List.length(#pats(row(m,0)))
       fun isEmpty(MATRIX{rows=[], ...}) = true
         | isEmpty _ = false

       fun removeFirstRow(MATRIX{rows=_::rows, paths}) = 
             MATRIX{rows=rows, paths=paths}
         | removeFirstRow _ = error "removeFirstRow"

       fun check(MATRIX{rows, paths, ...}) =
       let val arity = length paths
       in  app (fn {pats, ...} =>
                 if length pats <> arity then bug "bad matrix" else ())
               rows
       end

       fun toString(MATRIX{rows, paths, ...}) =
           listify("","\n","\n")
             (map (fn {pats, ...} =>
                    listify("[","\t","]") (map Pat.toString pats)) rows)

       (*
        * Given a matrix, find the best column for matching.
        *
        * I'm using the heuristic that John (Reppy) uses:
        * the first column i where pat_i0 is not a wild card, and
        * with the maximum number of distinct constructors in the
        * the column. 
        *
        * If the first row is all wild card, then return NONE.
        *)
       fun findBestMatchColumn(m as MATRIX{rows, ...}) = 
       let val _ = if sanityCheck then check m else ()
           val _ = if debug then
                      (print(toString m))
                   else ()
           val nCol = columnCount m

           fun score i = (* score of doing pattern matching on column i *)
           let val pats_i = col(m, i)
               val pats_i0 = hd pats_i 
           in  case pats_i0 of 
                 WILDpat => 0
               | _  =>
                 let val (cons, score) =
                    (* count distinct constructors; skip refutable cards 
                     * Give records, tuples and or pats, high scores so that
                     * they are immediately expanded
                     *)
                       List.foldr (fn (WILDpat, (S, n)) => (S, n)
                                    | (APPpat(c, _), (S, n)) => 
                                         (Decon.Set.add(S, c), n)
                                    | (_, (S, n)) => (S, 10000))
                           (Decon.Set.empty, 0) pats_i
                 in score + Decon.Set.numItems cons end
           end

           (* Find column with the highest score *)
           fun findBest(i, bestSoFar) =
               if i >= nCol then bestSoFar else 
               let val score_i = score i
                   val best = 
                       if case bestSoFar of
                            NONE                => true
                          | SOME(_, best_score) => score_i > best_score
                       then SOME(i, score_i)
                       else bestSoFar
               in  findBest(i+1, best)
               end

       in  case findBest(0, NONE) of
             SOME(i, 0) => NONE   (* a score of zero means all wildcards *)
           | SOME(i, _) => SOME i
           | NONE => NONE 
       end

   end (* Matrix *)

   val toString = DFA.toString

  (*
    * Rename user pattern into internal pattern.
    * The path business is hidden from the client.
    *)
   fun rename doIt {number=rule_no, pats, guard, action, cont} =
   let val empty = Subst.empty

       fun bind(subst, v, p) = 
           case Subst.find(subst, v) of
             SOME _ => error("duplicated pattern variable "^Var.toString v)
           | NONE => Subst.insert(subst, v, PVAR p)

       fun process(path, subst:subst, pat) : compiled_pat = 
       let fun idPat id = (WILDpat, bind(subst, id, path))
           fun asPat(id, p) = 
           let val (p, subst) = process(path, subst, p)
           in  (p, bind(subst, id, path))
           end
           fun wildPat() = (WILDpat, subst)
           fun litPat(lit) = (APPpat(LIT lit, []), subst)

           fun processPats(pats) = 
           let fun loop([], _, ps', subst) = (rev ps', subst)
                 | loop(p::ps, i, ps', subst) = 
                   let val path' = Path.dot(path, INT i)
                       val (p, subst) = process(path', subst, p)
                   in  loop(ps, i+1, p::ps', subst)
                   end
           in  loop(pats, 0, [], subst) end

           fun processLPats(lpats) = 
           let fun loop([], ps', subst) = (rev ps', subst)
                 | loop((l,p)::ps, ps', subst) = 
                   let val path' = Path.dot(path, LABEL l)
                       val (p, subst) = process(path', subst, p)
                   in  loop(ps, (l,p)::ps', subst)
                   end
           in  loop(lpats, [], subst) end
 
           fun consPat(c,args) : compiled_pat = 
           let val (pats, subst) = processPats(args)
           in  (* arity check *)
               if Con.arity c <> length args 
               then error("arity mismatch "^Con.toString c)
               else ();
               (APPpat(CON c, pats), subst) 
           end

           fun tuplePat(pats) : compiled_pat = 
           let val (pats, subst) = processPats(pats)
           in  (TUPLEpat pats, subst) end

           fun recordPat(lpats) : compiled_pat = 
           let val (lpats, subst) = processLPats(lpats)
           in  (RECORDpat lpats, subst) end

           fun noDupl(subst, subst') =
           let val duplicated =
                    VarSet.listItems( 
                     VarSet.intersection
                        (VarSet.addList(VarSet.empty, Subst.listKeys subst'),
                         VarSet.addList(VarSet.empty, Subst.listKeys subst)))
           in  case duplicated of
                 [] => ()
               | _ => error("duplicated pattern variables: "^
                            listify("",",","") (map Var.toString duplicated))
           end

           (* Or patterns are tricky because the same variable name
            * may be bound to different components.  We handle this by renaming
            * all variables to some canonical set of paths, 
            * then rename all variables to these paths. 
            *)
           fun logicalPat (name, name2, f)  [] = error("empty "^name^" pattern")
             | logicalPat (name, name2, f)  pats = 
           let val results  = map (fn p => process(path, empty, p)) pats
               val ps       = map #1 results
               val orSubsts = map #2 results
               fun sameVars([], s') = true
                 | sameVars(s::ss, s') = 
                   forall (fn (x,y) => Var.compare(x,y) = EQUAL) 
                      (Subst.listKeys s, s') andalso
                        sameVars(ss, s')
               (* make sure all patterns use the same set of
                * variable names
                *)
               val orNames = Subst.listKeys(hd orSubsts)
               val _ = if sameVars(tl orSubsts, orNames) then ()
                       else error("not all "^name2^
                                  " have the same variable bindings")
               val _ = noDupl(subst, hd orSubsts)
               (* build the new substitution to include all names in the    
                * or patterns.
                *)

               val subst = Subst.foldri  
                            (fn (v, _, subst) => Subst.insert(subst,v,VAR v)
                            ) subst (hd orSubsts) 
           in  (f(ListPair.zip(orSubsts,ps)), subst)
           end

           fun orPat pats = logicalPat ("or", "disjuncts", ORpat) pats
           fun andPat pats = logicalPat ("and", "conjuncts", ANDpat) pats

           fun notPat pat = 
           let val (pat,subst')  = process(path, empty, pat)
               val _ = noDupl(subst,subst')
           in  (NOTpat(subst',pat), subst)
           end

           fun wherePat(pat, e) =
           let val (pat, subst') = process(path, empty, pat)
               val _ = noDupl(subst,subst')
           in  (WHEREpat(pat, subst', e), subst)
           end

           fun nestedPat(pat1, e, pat2) =
           let val path' = Path.dot(path, INT ~1)
               val (pat1, subst1) = process(path, subst, pat1)
               val (pat2, subst2) = process(path',subst1, pat2)
           in  (NESTEDpat(pat1, subst1, path', e, pat2), subst2)
           end 

       in  doIt {idPat=idPat,
                 asPat=asPat,
                 wildPat=wildPat,
                 consPat=consPat,
                 tuplePat=tuplePat,
                 recordPat=recordPat,
                 litPat=litPat,
                 orPat=orPat,
                 andPat=andPat,
                 notPat=notPat,
                 wherePat=wherePat,
                 nestedPat=nestedPat
                } pat
       end

       fun processAllPats(i, [], subst, ps') = (rev ps', subst)
         | processAllPats(i, p::ps, subst, ps') =
           let val (p, subst) = process(PATH[INT i], subst, p)
           in  processAllPats(i+1, ps, subst, p::ps')  end

       val (pats, subst) = processAllPats(0, pats, empty, [])  
   in  (rule_no, pats, guard, subst, action)
   end

   structure DFAMap = 
      RedBlackMapFn(type ord_key = dfa 
                    fun st(DFA{stamp, ...}) = stamp
                    fun compare(x,y) = Int.compare(st x, st y)
                   )

   (*
    * Give the arguments to case, factor out the common case and make it 
    * the default.
    *)
   fun factorCase(p, cases, d as SOME _) = (p, cases, d)
     | factorCase(p, cases, NONE) = 
       let fun count(m,dfa) = getOpt(DFAMap.find(m,dfa),0)
           fun inc((_,_,dfa),m) = DFAMap.insert(m, dfa, 1 + count(m, dfa))
           val m = foldr inc DFAMap.empty cases
           val best = DFAMap.foldri 
                   (fn (dfa,c,NONE) => SOME(dfa,c)
                     | (dfa,c,best as SOME(_,c')) =>
                       if c > c' then SOME(dfa,c) else best)
                      NONE m  
           fun neq(DFA{stamp=x, ...},DFA{stamp=y,...}) = x<>y
       in  case best of
             NONE => (p, cases, NONE) 
           | SOME(_,1) => (p, cases, NONE) 
           | SOME(defaultCase,n) => 
             let val others = List.filter(fn (_,_,x) => neq(x,defaultCase))
                                cases
             in  (p, others, SOME defaultCase) 
             end
       end 

   (* 
    * The main pattern matching compiler.
    * The dfa states are constructed with hash consing at the same time
    * so no separate DFA minimization step is needed.
    *)
   fun compile{compiled_rules, compress} =
   let exception NoSuchState

       datatype expandType = SWITCH of (decon * path list * matrix) list 
                                     * matrix option
                           | PROJECT of path * (path * index) list * matrix

       fun simp x = if compress then factorCase x else x

       (* Table for hash consing *)
       val dfaTable = DFA.HashTable.mkTable(32,NoSuchState) :
                           dfa DFA.HashTable.hash_table
       val lookupState = DFA.HashTable.lookup dfaTable
       val insertState = DFA.HashTable.insert dfaTable

       val stampCounter = ref 0

       fun mkState(test) =   
       let val stamp = !stampCounter
       in  stampCounter := stamp + 1;
           DFA{stamp=stamp, freeVars=ref Name.Set.empty, 
               height=ref 0, refCount=ref 0, generated=ref false, test=test}
       end

       fun newState test =
       let val s = mkState(test)
       in  lookupState s handle NoSuchState => (insertState(s, s); s)
       end

       (* State constructors *)
       val fail = newState(FAIL)
       fun Ok x = newState(OK x)
       fun Case(_, [], SOME x) = x
         | Case(_, [], NONE) = fail
         | Case(p, cases as (_,_,c)::cs, default) = 
           if List.all(fn (_,_,c') => DFA.eq(c,c')) cs andalso
              (case default of
                 SOME x => DFA.eq(c,x)     
               | NONE => true
              )
           then c
           else newState(CASE(simp(p, cases, default)))
       fun Select(x) = newState(SELECT(x))
       fun Cont(x) = newState(CONT(x))
       fun Where(g, yes, no) = 
           if DFA.eq(yes,no) then yes else newState(WHERE(g, yes, no))
       fun Bind(subst, x) =
           if Subst.numItems subst = 0 then x else newState(BIND(subst, x))
       fun Let x = newState(LET x)

       (*
        * Expand column i, 
        * Return a new list of matrixes indexed by the deconstructors.
        *) 
       fun expandColumn(m as MATRIX{rows, paths, ...}, i) = 
       let val ithCol = Matrix.col(m, i)
           val path_i = Matrix.pathOf(m, i)
           val _ = if debug then
                      (print("Expanding column "^i2s i^"\n"))
                   else ()
 
           fun split_i ps =
           let fun loop(j, p::ps, ps') =
                   if i = j then (rev ps', p, ps) 
                   else loop(j+1, ps, p::ps')
                 | loop _ = bug "split_i"
           in  loop(0, ps, []) end
 
           (* If the ith column cfind out what to expand *)
           fun expand(WILDpat::ps, this) = expand(ps, this)
             | expand((p as ORpat _)::ps, this) = SOME p
             | expand((p as ANDpat _)::ps, this) = SOME p
             | expand((p as NOTpat _)::ps, this) = SOME p
             | expand((p as WHEREpat _)::ps, this) = SOME p
             | expand((p as NESTEDpat _)::ps, this) = SOME p
             | expand((p as CONTpat _)::ps, this) = SOME p
             | expand((p as TUPLEpat _)::ps, this) = expand(ps, SOME p)
             | expand((p as RECORDpat _)::ps, this) = expand(ps, SOME p)
             | expand((p as APPpat _)::ps, this) = expand(ps, SOME p)
             | expand([], this) = this

            (* Split the paths *)
           val (prevPaths, _, nextPaths) = split_i paths

       in  case expand(ithCol, NONE) of
             SOME(NOTpat _) => (* expand not patterns *)
             let fun expand([], _) = bug "expand NOT" 
                   | expand((row as {pats, guard, nested, dfa})::rows,rows') = 
                 let val (prev, pat_i, next) = split_i(pats)
                 in  case pat_i of
                       NOTpat(subst,p) =>
                           let val rows' = rev rows'
                               val yes   = {pats=prev@[WILDpat]@next,
                                            nested=nested,
                                            guard=guard, dfa=dfa}
                               val m2 = MATRIX{rows=rows, paths=paths}
                               val no = {pats=prev@[p]@next, guard=NONE, 
                                         nested=[],
                                         dfa=Bind(subst,match m2)}
                               val m1 = MATRIX{rows=rows'@[no,yes]@rows,
                                               paths=paths}
                           in  expandColumn(m1, i) end
                         | _ => expand(rows, row::rows')
                     end   
             in  expand(rows, [])
             end
           | SOME(ORpat _ | WHEREpat _ | NESTEDpat _) => 
                (* if we have or/where patterns then expand all rows
                 * with these patterns
                 *)
             let fun expand(row as {pats, dfa, nested, guard}) =
                 let val (prev, pat_i, next) = split_i(pats)
                 in  case pat_i of
                       ORpat ps =>
                         map (fn (subst,p) => 
                                {pats=prev@[p]@next, nested=nested,
                                 dfa=Bind(subst,dfa), guard=guard})
                             ps
                     | WHEREpat(p,subst',g) =>
                        [{pats=prev@[p]@next, dfa=dfa, nested=nested,
                          guard=case guard of
                                  NONE => SOME(subst',g)
                                | SOME(subst,g') => 
                                        SOME(mergeSubst(subst,subst'),
                                             Guard.logicalAnd(g,g'))
                         }]
                     | NESTEDpat(pat, subst, path, exp, pat') =>
                        [{pats=prev@[pat]@next, dfa=dfa,
                          nested=(subst,path,exp,pat')::nested,
                          guard=guard}]
                     | _ => [row]
                 end
                 val newMatrix =
                      MATRIX{rows  = List.concat (map expand rows),
                             paths = paths
                            }
             in  expandColumn(newMatrix, i)
             end
           | SOME(TUPLEpat pats) => (* expand a tuple along all the columns *)
             let val arity = length pats
                 val wilds = map (fn _ => WILDpat) pats
                 fun processRow{pats, nested, dfa, guard} =
                 let val (prev, pat_i, next) = split_i(pats)
                 in  case pat_i of
                        TUPLEpat ps' =>
                        let val n   = length ps'
                        in  if n <> arity then error("tuple arity mismatch")
                            else ();
                            {pats=prev @ ps' @ next, nested=nested,
                             dfa=dfa, guard=guard}
                        end
                     |  WILDpat => 
                           {pats=prev @ wilds @ next, nested=nested,
                            dfa=dfa,guard=guard}
                     |  pat => error("mixing tuple and: "^Pat.toString pat)
                 end
                 val rows  = map processRow rows
                 val path_i' = List.tabulate 
                                 (arity, fn i => Path.dot(path_i, INT i))
                 val paths = prevPaths @ path_i' @ nextPaths
                 val bindings = List.tabulate (arity, fn i => 
                                       (Path.dot(path_i, INT i), INT i))
             in  PROJECT(path_i,bindings,
                        MATRIX{rows=rows, paths=paths}
                       )
             end
           | SOME(RECORDpat _) => (* expand a tuple along all the columns *)
             let (* All the labels that are in this column *)
                 val labels = 
                     VarSet.listItems
                     (List.foldr 
                      (fn (RECORDpat lps, L) => 
                            List.foldr (fn ((l,p), L) => VarSet.add(L,l)) L lps
                        | (_, L) => L)
                        VarSet.empty ithCol)

                 val _ = if debug then
                            print("Labels="^listify("",",","") 
                                     (map Var.toString labels)^"\n")
                         else ()

                 fun lp2s(l,p) = Var.toString l^"="^Pat.toString p
                 fun lps2s lps = listify("","\t","") (map lp2s lps)
                 fun ps2s ps = listify("","\t","") (map Pat.toString ps)

                 val wilds = map (fn _ => WILDpat) labels

                 fun processRow{pats, nested, dfa, guard} =
                 let val (prev, pat_i, next) = split_i(pats)
                 in  case pat_i of
                        RECORDpat lps =>
                        (* Put lps in canonical order *)
                        let val lps = Pat.sortByLabel lps
                            val _   = if debug then
                                         print("lpats="^lps2s lps^"\n")
                                      else ()
 
                            fun collect([], [], ps') = rev ps'
                              | collect(x::xs, [], ps') = 
                                   collect(xs, [], WILDpat::ps')
                              | collect(x::xs, this as (l,p)::lps, ps') =
                                (case Var.compare(x,l) of
                                  EQUAL => collect(xs, lps, p::ps')
                                | LESS  => collect(xs, this, WILDpat::ps')
                                | GREATER => error "labels out of order"
                                )
                              | collect _ = bug "processRow"
                            val ps = collect(labels, lps, [])
                            val _   = if debug then
                                         print("new pats="^ps2s ps^"\n")
                                      else ()
                        in  {pats=prev @ ps @ next, nested=nested,
                             dfa=dfa, guard=guard}
                        end
                     |  WILDpat => 
                          {pats=prev @ wilds @ next,nested=nested,
                           dfa=dfa,guard=guard}
                     |  pat => error("mixing record and: "^Pat.toString pat)
                 end
                  
                 val rows  = map processRow rows

                 val path_i' = map (fn l => Path.dot(path_i, LABEL l)) labels
                 val paths = prevPaths @ path_i' @ nextPaths

                 val bindings = map (fn l => 
                                       (Path.dot(path_i, LABEL l), LABEL l))
                                     labels
             in  PROJECT(path_i,bindings,
                         MATRIX{rows=rows, paths=paths}
                        )
             end
           | SOME(APPpat(decon,_)) => 
           (* Find out how many variants are there in this case *)
             let fun getVariants() = 
                      Decon.Set.listItems 
                        (List.foldr 
                           (fn (APPpat(x,_),S) => Decon.Set.add(S,x)
                             | (_,S) => S) Decon.Set.empty ithCol)

                 val (allVariants, hasDefault) =
                   case decon of
                     CON c   => 
                       let val {known, others} = Con.variants c
                       in  (case known of [] => getVariants() 
                                        | _  => map CON known, others) 
                       end
                   | LIT l   => 
                      case Literal.variants l of
                        SOME{known, others} => (map LIT known, others)
                      | NONE => (getVariants(), true) 

                (* function from con -> matrix; initially no rows 
                 *)
                fun insert(tbl, key, x) = Decon.Map.insert(tbl, key, x)
                fun lookup(tbl, key) = 
                    case Decon.Map.find(tbl, key) of 
                      SOME x => x
                    | NONE => bug("can't find constructor "^Decon.toString key)
                val empty = Decon.Map.empty
     
                fun create([], tbl) = tbl
                  | create((con as CON c)::cons, tbl) =
                    let val n = Con.arity c
                        val paths = List.tabulate
                              (n, fn i => Path.dot(path_i, INT i))
                    in  create(cons, insert(tbl, con, {args=paths, rows=[]}))
                    end
                  | create((con as LIT l)::cons, tbl) =
                        create(cons, insert(tbl, con, {args=[], rows=[]}))
     
                val tbl = create(allVariants, empty)
     
                fun insertRow(tbl, decon, row) =
                    let val {args, rows} = lookup(tbl, decon)
                    in  insert(tbl, decon, {args=args, rows=rows@[row]})
                    end
    
                fun foreachRow([], tbl) = tbl
                  | foreachRow({pats, dfa, nested, guard}::rows, tbl) =
                    let val (prev, pat_i, next) = split_i pats
     
                        fun addRow(tbl, decon, pats) = 
                            insertRow(tbl, decon, 
                                   {pats=pats, nested=nested,
                                    dfa=dfa, guard=guard})
     
                        fun addWildToEveryRow(tbl) =
                            foldr (fn (c, tbl) => 
                                   let val {args, rows} = lookup(tbl, c)
                                       val wilds = map (fn _ => WILDpat) args
                                       val pats  = prev @ wilds @ next
                                   in  addRow(tbl, c, pats)
                                   end) tbl allVariants
      
                        val tbl = 
                           case pat_i of
                             WILDpat => addWildToEveryRow tbl
                           | APPpat(decon, args) =>
                             let val pats = prev @ args @ next
                             in  addRow(tbl, decon, pats)
                             end
                           | _ => error 
                             "expecting constructor but found tuple/record"
                    in  foreachRow(rows, tbl)
                    end
     
                val tbl = foreachRow(rows, tbl)
     
                fun collectCases(decon, {args, rows}, rules) = 
                let val matrix = 
                        MATRIX{rows=rows, paths=prevPaths @args@nextPaths}
                in  (decon, args, matrix)::rules
                end

                val cases = Decon.Map.foldri collectCases [] tbl

                (* If we have a default then the default matrix
                 * contains the original matrix with rows whose
                 * column i is the wild card.
                 *)
                val default =
                    if hasDefault then 
                       SOME(
                        MATRIX{rows=List.filter 
                                     (fn {pats, ...} =>
                                        case List.nth(pats, i) of
                                          WILDpat => true
                                        | _ => false) rows,
                               paths=paths}
                       )   
                    else NONE
     
             in  SWITCH(Decon.Map.foldri collectCases [] tbl, default)
             end
           | SOME p => bug ("expandColumn: "^Pat.toString p)
           | NONE => bug "expandColumn"
       end (* expandColumn *)

       (*
        * Generate the DFA
        *)
       and match matrix =
           if Matrix.isEmpty matrix then fail
           else
           case Matrix.findBestMatchColumn matrix of
             NONE =>   (* first row is all wild cards *) 
               (case Matrix.row(matrix, 0) of
                 {guard=SOME(subst, g), nested=[], dfa, ...} => 
                      (* generate guard *)
                   Bind(subst,
                       Where(g, dfa, 
                             match(Matrix.removeFirstRow matrix)))
               | {guard=NONE, dfa, nested=[], ...} => dfa
               | {guard, pats, nested=n::ns, dfa, ...} => 
                        (* handle nested pats *)
                 let val (subst, path, exp, pat) = n
                     val MATRIX{rows, paths} = matrix
                     val row0  = {guard=guard, pats=pat::pats,
                                  nested=ns, dfa=dfa}
                     val rows' = tl rows
                     val rows' = map (fn {pats, nested, dfa, guard} =>
                           {pats=WILDpat::pats, nested=nested, dfa=dfa,
                            guard=guard}) rows'
                     val m = MATRIX{rows=row0::rows', paths=path::paths}
                 in  Bind(subst, Let(path, exp, match m))
                 end
               )
           | SOME i => 
              (* mixture rule; split at column i *)
             (case expandColumn(matrix, i) of
               (* splitting a constructor *)
               SWITCH(cases, default) =>
               let val cases = map (fn (c,p,m) => (c,p,match m)) cases
               in  Case(Matrix.pathOf(matrix, i), cases, 
                        Option.map match default)
               end
               (* splitting a tuple or record;
                * recompute new bindings.
                *)
             | PROJECT(p,bindings,m) => Select(p, bindings, match m)
             )

       fun makeMatrix rules =
       let val (_, pats0, _, _, _) = hd rules
           val arity = length pats0
           fun makeRow(r, pats, NONE, subst, action) =
               {pats=pats, guard=NONE, nested=[],
                dfa=Bind(subst, Ok(r, action))}
             | makeRow(r, pats, SOME g, subst, action) = 
               {pats=pats, guard=SOME(subst,g), nested=[],
                dfa=Ok(r, action)}
             
       in  MATRIX{rows  = map makeRow rules,
                  paths = List.tabulate(arity, fn i => PATH[INT i])
                 }
       end

       val dfa = match(makeMatrix compiled_rules)

       val rule_nos = map #1 compiled_rules

       (*
        * 1. Update the reference counts. 
        * 2. Compute the set of free path variables at each state. 
        * 3. Compute the set of path variables that are actually used.
        * 4. Compute the height of each node.
        *)
       exception NotVisited
       val visited = IntHashTable.mkTable (32, NotVisited)
       fun mark s = IntHashTable.insert visited (s,true)
       fun isVisited s = getOpt(IntHashTable.find visited s, false)

       fun set(fv, s) = (fv := s; s)
       fun setH(height, h) = (height := h; h)
       val union = Name.Set.union
       val diff  = Name.Set.difference
       val add   = Name.Set.add
       val empty = Name.Set.empty

       fun diffPaths(fvs, ps) = 
           diff(fvs, Name.Set.addList(Name.Set.empty, map PVAR ps))

       val used = ref Name.Set.empty
       fun occurs s = used := Name.Set.union(!used,s)
       val redundant = ref(IntListSet.addList(IntListSet.empty, rule_nos))
       fun ruleUsed r = redundant := IntListSet.delete(!redundant, r)
       fun vars subst = Name.Set.addList(empty,Subst.listItems subst)

       fun visit(DFA{stamp, refCount, test, freeVars, height, ...},PVs) = 
           (refCount := !refCount + 1;
            if isVisited stamp then (!freeVars, !height)
            else (mark stamp;
                  case test of
                    FAIL => (empty, 0)
                  | BIND(subst, dfa) => 
                    let val patvars = Name.Set.addList(empty, 
                                        map VAR (Subst.listKeys subst))
                        val (s, h) = visit(dfa, union(PVs, patvars))
                        val variables = vars subst
                        val s' = union(s, variables)
                        val s' = diff(s', patvars) 
                    in  occurs s'; 
                        (set(freeVars, s'), setH(height, h + 1))
                    end
                  | LET(p, _, dfa) =>
                    let val (s, h) = visit(dfa, PVs)
                    in  (set(freeVars, s), setH(height, h+1))
                    end
                  | OK(rule_no, action) => 
                    let val fvs = Name.Set.addList(empty, 
                                    map VAR(Action.freeVars action))
                        (* val _ = 
                            (print("Action = "^Action.toString action^"\n");
                             print("PVs = "^Name.setToString PVs^"\n");
                             print("FVs = "^Name.setToString fvs^"\n")
                            ) *)
                        val fvs = Name.Set.intersection(PVs, fvs)
                    in  ruleUsed rule_no; 
                        (set(freeVars, fvs), 0)
                    end
                  | CASE(p, cases, opt) =>
                    let val (fvs, h) = 
                         List.foldr (fn ((_,ps,x),(s, h)) => 
                             let val (fv,h') = visit(x, PVs)
                                 val fv = diffPaths(fv, ps)
                             in  (union(fv,s), Int.max(h,h'))
                             end)
                             (empty, 0) cases 
                        val (fvs, h) =  
                            case opt of NONE => (fvs, h) 
                                      | SOME x => 
                                        let val (fv, h') = visit(x, PVs)
                                        in  (union(fvs,fv), Int.max(h,h'))
                                        end
                        val fvs = add(fvs, PVAR p) 
                    in  occurs fvs; 
                        (set(freeVars, fvs), setH(height, h+1))
                    end 
                  | WHERE(_, y, n) => 
                    let val (sy, hy) = visit(y, PVs)
                        val (sn, hn) = visit(n, PVs)
                        val s = union(sy, sn)
                        val h = Int.max(hy,hn) + 1
                    in  occurs s; 
                        (set(freeVars, s), setH(height, h))
                    end
                  | SELECT(p, bs, x) => 
                    let val (s, h) = visit(x, PVs)
                        val s  = add(s, PVAR p)
                        val bs = foldr (fn ((p,_),S) => add(S,PVAR p)) s bs 
                        val fvs = diff(s, bs)
                    in  occurs bs; 
                        (set(freeVars, fvs), setH(height,h+1)) 
                    end 
                  | CONT(k, x) =>
                    let val (s, h) = visit(x, PVs)
                    in  (* always generate a state function *)
                        refCount := !refCount + 1; 
                        (set(freeVars, s), setH(height,h+1))
                    end 
                 )
           )
       val _ = visit(dfa, empty); 
       val DFA{refCount=failCount, ...} = fail
   in  ROOT{used = !used, 
            dfa = dfa, 
            exhaustive= !failCount = 0, 
            redundant= !redundant
           }
   end

   fun exhaustive(ROOT{exhaustive, ...}) = exhaustive
   fun redundant(ROOT{redundant, ...}) = redundant

   (*
    * Generate final code for pattern matching.
    *)
   fun codeGen 
        { genFail : unit -> 'exp,
          genOk,   
          genPath,   
          genBind,   
          genCase,
          genIf   : Guard.guard * 'exp * 'exp -> 'exp,
          genGoto,
          genFun, 
          genLet  : 'decl list * 'exp -> 'exp,
          genProj : path * (path option * index) list -> 'decl,
          genVar  : path -> Var.var,
          genVal  : Var.var * 'exp -> 'decl,
          genCont 
        } (root, dfa) = 
   let
       val ROOT{dfa, used, ...} = dfa

       fun genPat p = if Name.Set.member(used, PVAR p) then SOME p else NONE 
       (* fun arg p = SOME p *)

       fun mkVars freeVarSet = 
           map (fn PVAR p => genVar p
                 | VAR v  => v
               ) (Name.Set.listItems (!freeVarSet))

       fun enque(dfa,(F,B)) = (F,dfa::B)
       val emptyQueue = ([], [])

       (* Walk a state, if it is shared then just generate a goto to the
        * state function; otherwise expand it 
        *)  
       fun walk(dfa as DFA{stamp, refCount, generated, freeVars, ...},
                           workList) = 
           if !refCount > 1 then 
              (* just generate a goto *)
              let val code = genGoto(stamp, mkVars freeVars)
              in  if !generated then (code, workList)
                  else (generated := true; (code, enque(dfa,workList)))
              end
           else
              expandDfa(dfa, workList) 

           (* generate a new function definition *)
       and genNewFun(dfa as DFA{stamp, freeVars, height, ...}, workList) =
           let val (body, workList) = expandDfa(dfa, workList)
           in  ((!height,genFun(stamp, mkVars freeVars, body)), workList) 
           end

       and expandYesNo(yes, no, workList) =
           let val (yes, workList) = walk(yes, workList)
               val (no, workList) = walk(no, workList)
           in  (yes, no, workList)
           end
 
           (* expand the dfa always *)
       and expandDfa(DFA{stamp, test, freeVars, ...}, workList) =  
              (case test of
                (* action *)
                OK(rule_no, action) => (genOk(action), workList)
                (* failure *)
              | FAIL => (genFail(), workList)
                (* guard *)
              | BIND(subst, dfa) =>
                let val (code, workList) = walk(dfa, workList)
                    val bindings = 
                       Subst.foldri 
                       (fn (v,PVAR p,b) => (v,genPath p)::b
                         | (v,VAR v',b) => b
                       ) [] subst
                in  (genLet(genBind bindings, code), workList)
                end
              | LET(path, (_, e), dfa) =>
                let val (code, workList) = walk(dfa, workList)
                in  (genLet(genBind [(genVar path,e)], code), workList)
                end
              | WHERE(g, yes, no) =>
                let val (yes, no, workList) = expandYesNo(yes, no, workList)
                in  (genIf(g, yes, no), workList)
                end
                (* case *)
              | CASE(path, cases, default) =>
                let val (cases, workList) = 
                      List.foldr 
                      (fn ((con, paths, dfa), (cases, workList)) =>
                           let val (code, workList) = walk(dfa, workList)
                           in  ((con, map genPat paths, code)::cases, workList) 
                           end
                      ) ([], workList) cases

                    (* find the most common case and make it the default *)

                    val (default, workList) = 
                        case default of
                          NONE => (NONE, workList)
                        | SOME dfa => 
                          let val (code, workList) = walk(dfa, workList)
                          in  (SOME code, workList) end
                                     
                in  (genCase(genVar path, cases, default), workList)
                end
              | SELECT(path, bindings, body) =>
                let val (body, workList) = walk(body, workList)
                    val bindings = map (fn (p,v) => (SOME p,v)) bindings
                in  (genLet([genProj(path, bindings)], body), workList)
                end
              | CONT(k, body) =>
                let val (body, workList) = walk(body, workList)
                in  (genLet([genCont(k, stamp, mkVars freeVars)],body),workList)
                end
              )

           (* Generate code for the dfa; accumulate all the auxiliary   
            * functions together and generate a let.
            *)
       fun genAll(root,dfa) =
           let val (exp, workList) = walk(dfa, emptyQueue)
               fun genAuxFunctions(([], []), funs) = funs   
                 | genAuxFunctions(([], B), funs) = 
                      genAuxFunctions((rev B,[]), funs)
                 | genAuxFunctions((dfa::F,B), funs) =
                   let val (newFun, workList) = genNewFun(dfa, (F, B))
                   in  genAuxFunctions(workList, newFun :: funs)
                   end
               val rootDecl = genVal(genVar(PATH [INT 0]), root)
               val funs = genAuxFunctions(workList, [])
               (* order the functions by dependencies; sort by lowest height *)
               val funs = ListMergeSort.sort
                           (fn ((h,_),(h',_)) => h > h') funs
               val funs = map #2 funs 
           in  genLet(rootDecl::funs, exp)
           end
   in  genAll(root,dfa)
   end

end

end (* local *)
