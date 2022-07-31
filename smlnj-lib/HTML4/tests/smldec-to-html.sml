(* ______________________________________________________________________
   smldec-to-html.sml
   ______________________________________________________________________ *)

structure Test = struct

structure H4U = HTML4Utils

structure H4T = HTML4Tokens

structure H4TU = HTML4TokenUtils

structure H4P = HTML4Parser

(* ____________________________________________________________ *)
(* Most of the following set of functions were automatically
   generated, with additional pattern matching and recursion added in an
   ad hoc fashion to arrive at a string H4U.parsetree that approximately
   shows a structure and the functions it defines.
 *)

local
    open Ast
in
    fun handleFixitem (handleItem : 'a -> string H4U.parsetree)
                      ({item, fixity, region} : 'a fixitem) =
        H4U.Nd(Atom.atom "fixity",
               [H4U.Lf (case fixity of SOME sym => Symbol.name sym
                                     | NONE => "NONE"),
                handleItem item])

    fun handleSigConst _ NoSig = H4U.Nd(Atom.atom "NoSig", nil)
      | handleSigConst handleElem (Opaque elem) =
        H4U.Nd(Atom.atom "Opaque", [handleElem elem])
      | handleSigConst handleElem (Transparent elem) =
        H4U.Nd(Atom.atom "Transparent", [handleElem elem])
    and handleExp (AndalsoExp _) = H4U.Nd(Atom.atom "AndalsoExp", nil)
      | handleExp (AppExp _) = H4U.Nd(Atom.atom "AppExp", nil)
      | handleExp (CaseExp _) = H4U.Nd(Atom.atom "CaseExp", nil)
      | handleExp (CharExp _) = H4U.Nd(Atom.atom "CharExp", nil)
      | handleExp (ConstraintExp _) = H4U.Nd(Atom.atom "ConstraintExp", nil)
      | handleExp (FlatAppExp exps) =
        H4U.Nd(Atom.atom "FlatAppExp", map (handleFixitem handleExp) exps)
      | handleExp (FnExp _) = H4U.Nd(Atom.atom "FnExp", nil)
      | handleExp (HandleExp _) = H4U.Nd(Atom.atom "HandleExp", nil)
      | handleExp (IfExp _) = H4U.Nd(Atom.atom "IfExp", nil)
      | handleExp (IntExp _) = H4U.Nd(Atom.atom "IntExp", nil)
      | handleExp (LetExp _) = H4U.Nd(Atom.atom "LetExp", nil)
      | handleExp (ListExp _) = H4U.Nd(Atom.atom "ListExp", nil)
      | handleExp (MarkExp (theexp, _)) = H4U.Nd(Atom.atom "MarkExp",
                                                 [handleExp theexp])
      | handleExp (OrelseExp _) = H4U.Nd(Atom.atom "OrelseExp", nil)
      | handleExp (RaiseExp _) = H4U.Nd(Atom.atom "RaiseExp", nil)
      | handleExp (RealExp _) = H4U.Nd(Atom.atom "RealExp", nil)
      | handleExp (RecordExp _) = H4U.Nd(Atom.atom "RecordExp", nil)
      | handleExp (SelectorExp _) = H4U.Nd(Atom.atom "SelectorExp", nil)
      | handleExp (SeqExp _) = H4U.Nd(Atom.atom "SeqExp", nil)
      | handleExp (StringExp _) = H4U.Nd(Atom.atom "StringExp", nil)
      | handleExp (TupleExp _) = H4U.Nd(Atom.atom "TupleExp", nil)
      | handleExp (VarExp _) = H4U.Nd(Atom.atom "VarExp", nil)
      | handleExp (VectorExp _) = H4U.Nd(Atom.atom "VectorExp", nil)
      | handleExp (WhileExp _) = H4U.Nd(Atom.atom "WhileExp", nil)
      | handleExp (WordExp _) = H4U.Nd(Atom.atom "WordExp", nil)
    and handleRule (Rule _) = H4U.Nd(Atom.atom "Rule", nil)
    and handlePat (AppPat _) = H4U.Nd(Atom.atom "AppPat", nil)
      | handlePat (CharPat _) = H4U.Nd(Atom.atom "CharPat", nil)
      | handlePat (ConstraintPat _) = H4U.Nd(Atom.atom "ConstraintPat", nil)
      | handlePat (FlatAppPat _) = H4U.Nd(Atom.atom "FlatAppPat", nil)
      | handlePat (IntPat _) = H4U.Nd(Atom.atom "IntPat", nil)
      | handlePat (LayeredPat _) = H4U.Nd(Atom.atom "LayeredPat", nil)
      | handlePat (ListPat _) = H4U.Nd(Atom.atom "ListPat", nil)
      | handlePat (MarkPat _) = H4U.Nd(Atom.atom "MarkPat", nil)
      | handlePat (OrPat _) = H4U.Nd(Atom.atom "OrPat", nil)
      | handlePat (RecordPat _) = H4U.Nd(Atom.atom "RecordPat", nil)
      | handlePat (StringPat _) = H4U.Nd(Atom.atom "StringPat", nil)
      | handlePat (TuplePat _) = H4U.Nd(Atom.atom "TuplePat", nil)
      | handlePat (VarPat _) = H4U.Nd(Atom.atom "VarPat", nil)
      | handlePat (VectorPat _) = H4U.Nd(Atom.atom "VectorPat", nil)
      | handlePat WildPat = H4U.Nd(Atom.atom "WildPat", nil)
      | handlePat (WordPat _) = H4U.Nd(Atom.atom "WordPat", nil)
    and handleStrexp (AppStr _) = H4U.Nd(Atom.atom "AppStr", nil)
      | handleStrexp (AppStrI _) = H4U.Nd(Atom.atom "AppStrI", nil)
      | handleStrexp (BaseStr thedec) = H4U.Nd(Atom.atom "BaseStr",
                                               [handleDec thedec])
      | handleStrexp (ConstrainedStr _) = H4U.Nd(Atom.atom "ConstrainedStr",
                                                 nil)
      | handleStrexp (LetStr _) = H4U.Nd(Atom.atom "LetStr", nil)
      | handleStrexp (MarkStr (thestr, _)) = H4U.Nd(Atom.atom "MarkStr",
                                                    [handleStrexp thestr])
      | handleStrexp (VarStr _) = H4U.Nd(Atom.atom "VarStr", nil)
    and handleFctexp (AppFct _) = H4U.Nd(Atom.atom "AppFct", nil)
      | handleFctexp (BaseFct _) = H4U.Nd(Atom.atom "BaseFct", nil)
      | handleFctexp (LetFct _) = H4U.Nd(Atom.atom "LetFct", nil)
      | handleFctexp (MarkFct _) = H4U.Nd(Atom.atom "MarkFct", nil)
      | handleFctexp (VarFct _) = H4U.Nd(Atom.atom "VarFct", nil)
    and handleWherespec (WhStruct _) = H4U.Nd(Atom.atom "WhStruct", nil)
      | handleWherespec (WhType _) = H4U.Nd(Atom.atom "WhType", nil)
    and handleSigexp (AugSig _) = H4U.Nd(Atom.atom "AugSig", nil)
      | handleSigexp (BaseSig _) = H4U.Nd(Atom.atom "BaseSig", nil)
      | handleSigexp (MarkSig _) = H4U.Nd(Atom.atom "MarkSig", nil)
      | handleSigexp (VarSig _) = H4U.Nd(Atom.atom "VarSig", nil)
    and handleFsigexp (BaseFsig _) = H4U.Nd(Atom.atom "BaseFsig", nil)
      | handleFsigexp (MarkFsig _) = H4U.Nd(Atom.atom "MarkFsig", nil)
      | handleFsigexp (VarFsig _) = H4U.Nd(Atom.atom "VarFsig", nil)
    and handleSpec (DataSpec _) = H4U.Nd(Atom.atom "DataSpec", nil)
      | handleSpec (ExceSpec _) = H4U.Nd(Atom.atom "ExceSpec", nil)
      | handleSpec (FctSpec _) = H4U.Nd(Atom.atom "FctSpec", nil)
      | handleSpec (IncludeSpec _) = H4U.Nd(Atom.atom "IncludeSpec", nil)
      | handleSpec (MarkSpec _) = H4U.Nd(Atom.atom "MarkSpec", nil)
      | handleSpec (ShareStrSpec _) = H4U.Nd(Atom.atom "ShareStrSpec", nil)
      | handleSpec (ShareTycSpec _) = H4U.Nd(Atom.atom "ShareTycSpec", nil)
      | handleSpec (StrSpec _) = H4U.Nd(Atom.atom "StrSpec", nil)
      | handleSpec (TycSpec _) = H4U.Nd(Atom.atom "TycSpec", nil)
      | handleSpec (ValSpec _) = H4U.Nd(Atom.atom "ValSpec", nil)
    and handleDec (AbsDec _) = H4U.Nd(Atom.atom "AbsDec", nil)
      | handleDec (AbstypeDec _) = H4U.Nd(Atom.atom "AbstypeDec", nil)
      | handleDec (DatatypeDec _) = H4U.Nd(Atom.atom "DatatypeDec", nil)
      | handleDec (ExceptionDec _) = H4U.Nd(Atom.atom "ExceptionDec", nil)
      | handleDec (FctDec _) = H4U.Nd(Atom.atom "FctDec", nil)
      | handleDec (FixDec _) = H4U.Nd(Atom.atom "FixDec", nil)
      | handleDec (FsigDec _) = H4U.Nd(Atom.atom "FsigDec", nil)
      | handleDec (FunDec (fbs, tyvars)) =
        H4U.Nd(Atom.atom "FunDec", [H4U.Nd(Atom.atom "fbs", map handleFb fbs),
                                    H4U.Nd(Atom.atom "tyvars",
                                           map handleTyvar tyvars)])
      | handleDec (LocalDec (dec1, dec2)) =
        H4U.Nd(Atom.atom "LocalDec", [handleDec dec1, handleDec dec2])
      | handleDec (MarkDec (thedec, _)) = H4U.Nd(Atom.atom "MarkDec",
                                                 [handleDec thedec])
      | handleDec (OpenDec _) = H4U.Nd(Atom.atom "OpenDec", nil)
      | handleDec (OvldDec _) = H4U.Nd(Atom.atom "OvldDec", nil)
      | handleDec (SeqDec decs) = H4U.Nd(Atom.atom "SeqDec",
                                         map handleDec decs)
      | handleDec (SigDec sigbs) = H4U.Nd(Atom.atom "SigDec",
                                          map handleSigb sigbs)
      | handleDec (StrDec strbs) = H4U.Nd(Atom.atom "StrDec",
                                          map handleStrb strbs)
      | handleDec (TypeDec _) = H4U.Nd(Atom.atom "TypeDec", nil)
      | handleDec (ValDec _) = H4U.Nd(Atom.atom "ValDec", nil)
      | handleDec (ValrecDec _) = H4U.Nd(Atom.atom "ValrecDec", nil)
    and handleVb (MarkVb _) = H4U.Nd(Atom.atom "MarkVb", nil)
      | handleVb (Vb _) = H4U.Nd(Atom.atom "Vb", nil)
    and handleRvb (MarkRvb _) = H4U.Nd(Atom.atom "MarkRvb", nil)
      | handleRvb (Rvb _) = H4U.Nd(Atom.atom "Rvb", nil)
    and handleFb (Fb (clauses, flag)) =
        H4U.Nd(Atom.atom "Fb", (map handleClause clauses) @
                               [if flag then H4U.Lf "true"
                                else H4U.Lf "false"])
      | handleFb (MarkFb (thefb, _)) = H4U.Nd(Atom.atom "MarkFb",
                                              [handleFb thefb])
    and handleClause (Clause {exp, pats, resultty}) =
        H4U.Nd(Atom.atom "Clause", [
               H4U.Nd (Atom.atom "pats", map (handleFixitem handlePat) pats),
               H4U.Nd (Atom.atom "exp", [handleExp exp]),
               H4U.Nd (Atom.atom "resultty", [
                       case resultty of SOME tyast => handleTy tyast
                                      | NONE => H4U.Lf "NONE"])
              ])
    and handleTb (MarkTb _) = H4U.Nd(Atom.atom "MarkTb", nil)
      | handleTb (Tb _) = H4U.Nd(Atom.atom "Tb", nil)
    and handleDb (Db _) = H4U.Nd(Atom.atom "Db", nil)
      | handleDb (MarkDb _) = H4U.Nd(Atom.atom "MarkDb", nil)
    and handleDbrhs (Constrs _) = H4U.Nd(Atom.atom "Constrs", nil)
      | handleDbrhs (Repl _) = H4U.Nd(Atom.atom "Repl", nil)
    and handleEb (EbDef _) = H4U.Nd(Atom.atom "EbDef", nil)
      | handleEb (EbGen _) = H4U.Nd(Atom.atom "EbGen", nil)
      | handleEb (MarkEb _) = H4U.Nd(Atom.atom "MarkEb", nil)
    and handleStrb (MarkStrb (thestrb, _)) = H4U.Nd(Atom.atom "MarkStrb",
                                                    [handleStrb thestrb])
      | handleStrb (Strb {name, constraint, def}) =
        H4U.Nd(Atom.atom "Strb",
               [H4U.Nd(Atom.atom "name", [H4U.Lf (Symbol.name name)]),
                H4U.Nd(Atom.atom "constraint", [handleSigConst handleSigexp
                                                               constraint]),
                H4U.Nd(Atom.atom "def", [handleStrexp def])])
    and handleFctb (Fctb _) = H4U.Nd(Atom.atom "Fctb", nil)
      | handleFctb (MarkFctb _) = H4U.Nd(Atom.atom "MarkFctb", nil)
    and handleSigb (MarkSigb _) = H4U.Nd(Atom.atom "MarkSigb", nil)
      | handleSigb (Sigb _) = H4U.Nd(Atom.atom "Sigb", nil)
    and handleFsigb (Fsigb _) = H4U.Nd(Atom.atom "Fsigb", nil)
      | handleFsigb (MarkFsigb _) = H4U.Nd(Atom.atom "MarkFsigb", nil)
    and handleTyvar (MarkTyv (thetyv, _)) = H4U.Nd(Atom.atom "MarkTyv",
                                                   [handleTyvar thetyv])
      | handleTyvar (Tyv _) = H4U.Nd(Atom.atom "Tyv", nil)
    and handleTy (ConTy _) = H4U.Nd(Atom.atom "ConTy", nil)
      | handleTy (MarkTy _) = H4U.Nd(Atom.atom "MarkTy", nil)
      | handleTy (RecordTy _) = H4U.Nd(Atom.atom "RecordTy", nil)
      | handleTy (TupleTy _) = H4U.Nd(Atom.atom "TupleTy", nil)
      | handleTy (VarTy _) = H4U.Nd(Atom.atom "VarTy", nil)
end

(* ____________________________________________________________ *)

val tokIsSpace = H4P.tokIsSpace

fun filterSpaceFromParseStream strm =
    let fun pred (H4U.VisitT tok) = not (tokIsSpace tok)
          | pred _ = true
    in H4U.stream_filter pred strm end

fun tokIsOpenTag tok = String.isPrefix "START" (HTML4Tokens.toString tok)

fun tokIsCloseTag tok = String.isPrefix "END" (HTML4Tokens.toString tok)

(* ____________________________________________________________ *)

val templateStream =
    let val instrm = TextIO.openIn "template.html"
        val template_pt_opt = HTML4Parser.parseStream instrm
    in
        TextIO.closeIn instrm;
        case template_pt_opt of
            SOME (H4U.Nd(_, children)) =>
            H4U.stream_concatl (map H4U.parsetreeToVisitationStream children)
          | _ => H4U.StreamNil
    end handle ex => H4U.StreamNil

(* ____________________________________________________________ *)

exception IllFormedHTMLParseStream of H4T.token H4U.parsevisitation H4U.stream

fun outputHTMLParseStream (istrm, ostrm) =
    let fun visit (H4U.EnterNT _, indent) = indent ^ " "
          | visit (H4U.ExitNT _, indent) = String.extract(indent, 1, NONE)
          | visit (H4U.VisitT tok, indent) =
            (TextIO.output(ostrm,
                           String.concat [indent, H4TU.tokToString tok, "\n"]);
             indent)
        val _ = H4U.stream_foldl visit "" istrm
    in () end

structure PP = PrettyPrint

fun ppHTMLParseStream ppstrm istrm =
    let fun visit (H4U.EnterNT _) =
            PP.openHVBox ppstrm (PP.Rel 2)
          | visit (H4U.ExitNT _) =
            PP.closeBox ppstrm
          | visit (H4U.VisitT tok) =
            (PP.string ppstrm (H4TU.tokToString tok);
             PP.cut ppstrm)
        val _ = H4U.stream_app visit istrm
    in () end

(* __________________________________________________ *)

(* The following was an attempt at a fancier pretty printer, but it
was not meant to be. *)

fun ppHTMLParseStream' ppstrm istrm =
    let exception BadStream
        fun do_closes 0 = ()
          | do_closes n = (PP.closeBox ppstrm; do_closes (n - 1))
        fun visit (H4U.EnterNT _, (opens, openstk)) =
            (PP.openHVBox ppstrm (PP.Rel 1); (1, opens::openstk))
          | visit (H4U.ExitNT _, (opens, opens'::openstk)) =
            (do_closes opens; (opens', openstk))
          | visit (H4U.ExitNT _, (_, [])) = raise BadStream
          | visit (H4U.VisitT tok, (opens, openstk)) =
            let val opens' = ref opens
            in
                if tokIsCloseTag tok then (
                    PP.closeBox ppstrm;
                    PP.newline ppstrm;
                    opens' := (!opens') - 1)
                else ();
                PP.string ppstrm (H4TU.tokToString tok);
                if tokIsOpenTag tok then (
                    PP.newline ppstrm;
                    PP.openHVBox ppstrm (PP.Rel 1);
                    opens' := (!opens') + 1)
                else PP.space ppstrm 1;
                (!opens', openstk)
            end
        val _ = H4U.stream_foldl visit (0,[]) istrm 
            handle BadStream => raise IllFormedHTMLParseStream istrm
    in () end

(* ____________________________________________________________ *)

exception NotPossible

structure CommentMap = ListMapFn(struct
                                 type ord_key = String.string
                                 val compare = String.compare
                                 end)

fun commentFilter commentMap =
    let fun guard (HTML4Tokens.COMMENT comStr) =
            CommentMap.inDomain(commentMap, comStr)
          | guard _ = false
        fun mapper (HTML4Tokens.COMMENT comStr) =
            CommentMap.lookup (commentMap, comStr)
          | mapper _ = raise NotPossible
    in H4U.parsetreeStreamMapTStream(guard, mapper) end

(* ____________________________________________________________ *)

fun parseFile filename =
    let val stream = TextIO.openIn filename
        val source = Source.newSource(filename, 1, stream, false,
                                      ErrorMsg.defaultConsumer())
        val result = SmlFile.parse source
    in Source.closeSource source; result end

(* ____________________________________________________________ *)

val aEm = Atom.atom "em"
val aUl = Atom.atom "ul"
val aLi = Atom.atom "li"

(* Here is a "simple" "little" example of many to many stream transduction. *)

fun scrubEmptyULs (orig as H4U.StreamCons(orig_enter as H4U.EnterNT ntAtom,
                                          tl_thunk)) =
    if Atom.same(aUl, ntAtom) then let
            val thunk_val = tl_thunk ()
        in case thunk_val of
               H4U.StreamCons(orig_start as H4U.VisitT (H4T.STARTUL _),
                              tl_thunk') =>
               let val thunk_val' = tl_thunk' ()
               in case thunk_val' of
                      H4U.StreamCons(H4U.VisitT H4T.ENDUL, tl_thunk'') =>
                      let val thunk_val'' = tl_thunk'' ()
                      in case thunk_val'' of
                             H4U.StreamCons(H4U.ExitNT ntAtom, tl_thunk''') =>
                             if Atom.same(aUl, ntAtom)
                             then scrubEmptyULs (tl_thunk'''())
                             else raise IllFormedHTMLParseStream orig
                           | _ => raise IllFormedHTMLParseStream orig
                      end
                    | _ => let
                          fun new_thunk' () = scrubEmptyULs thunk_val'
                          fun new_thunk () = H4U.StreamCons(orig_start,
                                                            new_thunk')
                      in H4U.StreamCons(orig_enter, new_thunk) end
               end
             | _ => raise IllFormedHTMLParseStream orig
        end
    else H4U.StreamCons(orig_enter, fn () => scrubEmptyULs (tl_thunk ()))
  | scrubEmptyULs (H4U.StreamCons (orig, tl_thunk)) =
    H4U.StreamCons(orig, fn () => scrubEmptyULs (tl_thunk ()))
  | scrubEmptyULs (orig as H4U.StreamNil) = orig

(* ____________________________________________________________ *)

fun handleFile filename =
    let val intree = parseFile filename
        val decStrm = H4U.parsetreeToVisitationStream (handleDec intree)
        fun ptStrmToHTMLPtStrm _ (H4U.EnterNT ntAtom) =
            H4U.stream_fromList [
            H4U.EnterNT aLi,
            H4U.VisitT (H4T.STARTLI("<li>", [])),
            H4U.EnterNT aEm,
            H4U.VisitT (H4T.STARTEM("<em>", [])),
            H4U.VisitT (H4T.PCDATA (Atom.toString ntAtom)),
            H4U.VisitT H4T.ENDEM,
            H4U.ExitNT aEm,
            H4U.EnterNT aUl,
            H4U.VisitT (H4T.STARTUL("<ul>", []))
            ]
          | ptStrmToHTMLPtStrm _ (H4U.ExitNT ntAtom) =
            H4U.stream_fromList [
            H4U.VisitT H4T.ENDUL,
            H4U.ExitNT aUl,
            H4U.VisitT H4T.ENDLI,
            H4U.ExitNT aLi
            ]
          | ptStrmToHTMLPtStrm tokToString (H4U.VisitT tok) =
            H4U.stream_fromList [
            H4U.EnterNT aLi,
            H4U.VisitT (H4T.STARTLI("<li>", [])),
            H4U.VisitT (H4T.PCDATA (tokToString tok)),
            H4U.VisitT H4T.ENDLI,
            H4U.ExitNT aLi
            ]
        val decHTMLStrm =
            H4U.stream_concatl [
            H4U.stream_fromList [H4U.EnterNT aUl,
                                 H4U.VisitT (H4T.STARTUL ("<ul", []))],
            scrubEmptyULs (H4U.stream_maps (ptStrmToHTMLPtStrm (fn x => x))
                                           decStrm),
            H4U.stream_fromList [H4U.VisitT H4T.ENDUL,
                                 H4U.ExitNT aUl]]
        val commentMap =
            foldl CommentMap.insert' CommentMap.empty
                  [("<!--title-->",
                    H4U.stream_singleton (H4U.VisitT (HTML4Tokens.PCDATA
                                                          filename))),
                   ("<!--filename-->",
                    H4U.stream_singleton (H4U.VisitT (HTML4Tokens.PCDATA
                                                          filename))),
                   ("<!--pt-->", decHTMLStrm)
                  ]
        val filterTemplate =
            (commentFilter commentMap) o filterSpaceFromParseStream
        val outstream = TextIO.openOut (filename ^ ".html")
        val _ = outputHTMLParseStream(filterTemplate templateStream, outstream)
    in TextIO.closeOut outstream end

(* ____________________________________________________________
   Main routine.
 *)

fun main (_, args) = (List.app handleFile args; OS.Process.success)
    handle ex => OS.Process.failure

end

(* ______________________________________________________________________
   End of smldec-to-html.sml
   ______________________________________________________________________ *)
