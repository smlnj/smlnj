(* diagnostic.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (http://www.smlnj.org)
 *
 * Print diagnostic messages when a type error occurs.
 *)

structure Diagnostic :> sig

  datatype site
    = NoSite
    | AtExpression of Absyn.exp
    | AtPattern of Absyn.pat
    | AtRule of Absyn.rule
    | AtValueBinding of Absyn.vb
    | AtRecursiveBinding of Absyn.rvb
    | AtDeclaration of Absyn.dec

  (* References below are to The Definition of Standard ML (Revised),
   * Section 4 (Static Semantics for the Core), unless qualified by an
   * appendix or by "SML/NJ extension."  For derived forms, the cited
   * appendix figure gives the translation and the cited rule is the one
   * that imposes the reported type equality.
   *
   * The extension references are documented in
   * doc/src/extensions/succ-ml.txt (Disjunctive Patterns) and
   * doc/src/smlnj-guide/successor-ml.adoc (Core Language Features);
   * vector syntax is also described in doc/src/extensions/extensions.txt.
   *)
  datatype mismatchKind
    = ApplicationArgument       (* Rule 8: application *)
    | ExpressionConstraint      (* Rule 9: typed expression *)
    | PatternConstraint         (* Rules 42-43: typed/layered patterns *)
    | OrPattern                 (* SML/NJ extension: Successor ML,
                                 * Disjunctive Patterns *)
    | ValueBinding              (* Rule 25: value binding *)
    | IfCondition               (* Appendix A, Figure 15; Rules 8, 13-14;
                                 * Appendix C, Figure 25: bool constructors *)
    | IfBranches                (* Appendix A, Figure 15; Rule 13 *)
    | WhileCondition            (* Appendix A, Figure 15; Rules 8, 13-14 *)
    | AndalsoOperand            (* Appendix A, Figure 15; Rules 8, 13-14 *)
    | OrelseOperand             (* Appendix A, Figure 15; Rules 8, 13-14 *)
    | MatchRules                (* Rules 13-14: match and match rule *)
    | CaseObject                (* Appendix A, Figure 15; Rules 8, 13-14 *)
    | HandlerDomain             (* Rule 10: handle expression *)
    | HandlerResult             (* Rule 10: handle expression *)
    | RaiseArgument             (* Rule 11: raise expression *)
    | ConstructorArgument       (* Rule 41: constructed pattern *)
    | FunctionConstraint        (* Appendix A, Figure 17; Rules 25-26, 42 *)
    | FunctionResult            (* Appendix A, Figure 17; Rule 9 *)
    | FunctionClauses           (* Appendix A, Figure 17; Rules 9, 13-14 *)
    | DoExpression              (* SML/NJ extension: do exp => val () = exp;
                                 * Rule 25 *)
    | VectorElements            (* SML/NJ extension: vector expressions *)
    | VectorPatternElements     (* SML/NJ extension: vector patterns *)

  datatype error
    = Mismatch of {
        kind: mismatchKind,
        left: Types.ty,
        right: Types.ty,
        failure: Unify.unifyFail,
        site: site,
        region: SourceMap.region
      }
    | NotFunction of {
        operatorTy: Types.ty,
        site: site,
        region: SourceMap.region
      }
    | MissingRecordField of {
        label: Symbol.symbol,
        recordTy: Types.ty,
        site: site,
        region: SourceMap.region
      }
    | UnresolvedFlexRecord of {
        ty: Types.ty,
        knownFields: Symbol.symbol list,
        region: SourceMap.region
      }
    | ExplicitTyvarGeneralization of {
        tyvar: Types.tyvar,
        region: SourceMap.region
      }
    | TopLevelExceptionTyvar of {region: SourceMap.region}

  val report : StaticEnv.staticEnv * ErrorMsg.errorFn * error -> unit
end = struct

  structure BT = BasicTypes
  structure PP = PrettyPrint
  structure S  = Symbol
  structure T  = Types
  structure TU = TypesUtil

  datatype site
    = NoSite
    | AtExpression of Absyn.exp
    | AtPattern of Absyn.pat
    | AtRule of Absyn.rule
    | AtValueBinding of Absyn.vb
    | AtRecursiveBinding of Absyn.rvb
    | AtDeclaration of Absyn.dec

  (* See the mismatchKind declaration above for the static-semantics
   * provenance of each constructor.
   *)
  datatype mismatchKind
    = ApplicationArgument
    | ExpressionConstraint
    | PatternConstraint
    | OrPattern
    | ValueBinding
    | IfCondition
    | IfBranches
    | WhileCondition
    | AndalsoOperand
    | OrelseOperand
    | MatchRules
    | CaseObject
    | HandlerDomain
    | HandlerResult
    | RaiseArgument
    | ConstructorArgument
    | FunctionConstraint
    | FunctionResult
    | FunctionClauses
    | DoExpression
    | VectorElements
    | VectorPatternElements

  datatype error
    = Mismatch of {
        kind: mismatchKind,
        left: Types.ty,
        right: Types.ty,
        failure: Unify.unifyFail,
        site: site,
        region: SourceMap.region
      }
    | NotFunction of {
        operatorTy: Types.ty,
        site: site,
        region: SourceMap.region
      }
    | MissingRecordField of {
        label: Symbol.symbol,
        recordTy: Types.ty,
        site: site,
        region: SourceMap.region
      }
    | UnresolvedFlexRecord of {
        ty: Types.ty,
        knownFields: Symbol.symbol list,
        region: SourceMap.region
      }
    | ExplicitTyvarGeneralization of {
        tyvar: Types.tyvar,
        region: SourceMap.region
      }
    | TopLevelExceptionTyvar of {region: SourceMap.region}

  (* Document layout *)

  (* A small document type for wrapping and highlighting type fragments. *)
  datatype piece
    = Plain of string
    | Highlight of string
    | SoftBreak of string

  datatype renderMode
    = Color
    | Marks
    | Unstyled

  type doc = piece list

  fun bug msg = ErrorMsg.impossible ("Diagnostic: " ^ msg)

  fun spaces n = StringCvt.padLeft #" " n ""

  fun markedChar c =
    if Char.isSpace c then #" " else #"^"

  fun anyMarked s =
    let val sz = size s
        fun lp i =
          if i < sz then String.sub (s, i) = #"^" orelse lp (i + 1) else false
    in  lp 0
    end

  fun text s : doc = [Plain s]
  fun mark s : doc = [Highlight s]
  fun soft s : doc = [SoftBreak s]

  fun join _ [] = []
    | join _ [x] = x
    | join sep (x :: xs) = x @ sep @ join sep xs

  fun nextChunkWidth [] = 0
    | nextChunkWidth (Plain s :: rest) =
        size s + nextChunkWidth rest
    | nextChunkWidth (Highlight s :: rest) =
        size s + nextChunkWidth rest
    | nextChunkWidth (SoftBreak _ :: _) = 0

  fun typeToString env width ty =
    let val ty =
          case TU.prune ty
            of T.VARty (ref (T.OVLDI _)) =>
                 List.nth (OverloadClasses.intClass, 0)
             | T.VARty (ref (T.OVLDW _)) =>
                 List.nth (OverloadClasses.wordClass, 0)
             | _ => ty
    in  PP.pp_to_string_sans width (PPType.ppType env) ty
    end

  fun tyconToString env width tycon =
    PP.pp_to_string_sans width (PPType.ppTycon env) tycon

  fun isArrowTyc tyc = TU.equalTycon (tyc, BT.arrowTycon)

  fun tyconNeedsParen tycon =
    isArrowTyc tycon
    orelse
    (case tycon of T.RECORDtyc _ => Tuples.isTUPLEtyc tycon | _ => false)

  fun tyNeedsParen ty =
    case TU.prune ty
      of T.CONty (tycon, _) => tyconNeedsParen tycon
       | T.MARKty (ty, _) => tyNeedsParen ty
       | _ => false

  fun hasLabel label = List.exists (fn label' => S.eq (label, label'))

  local
    val ansiHighlight = ANSITerm.toString [ANSITerm.FG ANSITerm.Red]
    val ansiReset = ANSITerm.toString []
  in
    fun color s = concat [ansiHighlight, s, ansiReset]
  end

  (* Convert a document into concrete text lines and matching highlighted lines.
   *)
  fun renderLines mode width prefix doc =
    let val prefixLen = size prefix
        val initMarks = spaces prefixLen
        val textLine = ref prefix
        val markLine = ref initMarks
        val column = ref prefixLen
        val lines = ref ([] : (string * string) list)

        fun emitLine () = (
          lines := (!textLine, !markLine) :: !lines;
          textLine := initMarks;
          markLine := initMarks;
          column := prefixLen
        )

        fun emitStringColor marked s = (
          textLine := !textLine ^
            (if marked then color s else s);
          column := !column + size s
        )

        fun emitStringMarked marked s = (
          textLine := !textLine ^ s;
          markLine := !markLine ^
            (if marked then String.map markedChar s else spaces (size s));
          column := !column + size s
        )

        fun emitStringUnstyled _ s = (
          textLine := !textLine ^ s;
          column := !column + size s
        )

        val emitString =
          case mode
            of Color => emitStringColor
             | Marks => emitStringMarked
             | Unstyled => emitStringUnstyled

        fun emitPieces [] = ()
          | emitPieces (Plain s :: rest) = (
              emitString false s;
              emitPieces rest
            )
          | emitPieces (Highlight s :: rest) = (
              emitString true s;
              emitPieces rest
            )
          | emitPieces (SoftBreak s :: rest) = (
              if !column > prefixLen
                 andalso !column + size s + nextChunkWidth rest > width then
                emitLine ()
              else
                emitString false s;
              emitPieces rest
            )
    in  emitPieces doc;
        rev ((!textLine, !markLine) :: !lines)
    end

  fun emitDoc mode stream (prefix, doc) =
    let val width = !Control_Print.lineWidth
        fun ppOne (last, (line, marks)) = (
          PP.string stream line;
          if anyMarked marks then
            (PP.newline stream; PP.string stream marks)
          else
            ();
          if last then () else PP.newline stream
        )
        fun loop [] = ()
          | loop [line] = ppOne (true, line)
          | loop (line :: rest) = (ppOne (false, line); loop rest)
    in  loop (renderLines mode width prefix doc)
    end

  (* Type comparisons *)

  datatype side = Left | Right
  fun choose Left (left, _) = left
    | choose Right (_, right) = right

  (* Render one side of a unification failure. *)
  fun renderTy side env (ty1, ty2, failure) =
    let val width = !Control_Print.lineWidth
        fun typeDoc highlight ty =
          (if highlight then mark else text) (typeToString env width ty)
        fun chooseSide pair = choose side pair

        (* UNK is irrelevant for diagnostic, so it becomes "_". *)
        fun renderArg (Unify.OK ty) = text (typeToString env width ty)
          | renderArg (Unify.UNK _) = text "_"
          | renderArg (Unify.ERR failure) = render false failure

        and argNeedsParen (Unify.ERR (ty1, ty2, failure)) =
              tyNeedsParen (chooseSide (ty1, ty2))
          | argNeedsParen _ = false

        and renderArgParen arg =
          if argNeedsParen arg then
            text "(" @ renderArg arg @ text ")"
          else
            renderArg arg

        and renderTuple args =
          join (soft " " @ text "* ") (map renderArgParen args)

        and renderRecord (labels, args) =
          let fun field (label, arg) =
                text (S.name label ^ ":") @ renderArg arg
              val fields = ListPair.mapEq field (labels, args)
          in  text "{" @ join (text "," @ soft " ") fields @ text "}"
          end

        and renderArrow [domain, range] =
              renderArgParen domain
              @ soft " " @ text "-> " @ renderArg range
          | renderArrow _ = bug "arrow tycon with wrong arity"

        and renderTypeArgs [] = []
          | renderTypeArgs [arg] = renderArgParen arg @ soft " "
          | renderTypeArgs args =
              text "("
              @ join (text "," @ soft " ") (map renderArg args)
              @ text ")" @ soft " "

        and renderContext {tycon, args} =
          if Tuples.isTUPLEtyc tycon then
            renderTuple args
          else if isArrowTyc tycon then
            renderArrow args
          else
            (case tycon
               of T.RECORDtyc labels => renderRecord (labels, args)
                | _ =>
                    renderTypeArgs args @ text (tyconToString env width tycon))

        and renderRecordTycon (labels, otherLabels) =
          let fun differs label = not (hasLabel label otherLabels)
              fun field label =
                let val l =
                      if differs label then
                        mark (S.name label)
                      else
                        text (S.name label)
                in  l @ text ":_"
                end
          in  if Tuples.isTUPLEtyc (T.RECORDtyc labels) then
                join (soft " " @ text "* ")
                  (map (fn label => if differs label then mark "_" else text "_") labels)
                @ soft " "
                @ let val n = length labels
                  in  text (concat ["(", Int.toString n, "-tuple)"])
                  end
              else
                text "{" @ join (text "," @ soft " ") (map field labels) @ text "}"
          end

        and render topLevel (ty1, ty2, failure) =
          case failure
            of Unify.CTX context => renderContext context
             | Unify.TYC (T.RECORDtyc labels1, T.RECORDtyc labels2, _, _) =>
                 renderRecordTycon
                   (chooseSide ((labels1, labels2), (labels2, labels1)))
             | _ =>
                 typeDoc (not topLevel) (chooseSide (ty1, ty2))
    in  render true (ty1, ty2, failure)
    end

  (* Mismatch descriptions *)

  type mismatchDescription = {
      headline: string,
      leftLabel: string,
      rightLabel: string
    }

  fun desc headline leftLabel rightLabel : mismatchDescription = {
      headline=headline,
      leftLabel=leftLabel,
      rightLabel=rightLabel
    }

  fun describeMismatch kind =
    case kind
      of ApplicationArgument =>
           desc "operator domain and operand do not agree" "domain" "operand"
       | ExpressionConstraint =>
           desc "expression does not match constraint" "expression" "constraint"
       | PatternConstraint =>
           desc "pattern and constraint do not agree" "pattern" "constraint"
       | OrPattern =>
           desc "or-patterns do not agree" "first pattern" "second pattern"
       | ValueBinding =>
           desc "pattern and expression in val declaration do not agree"
             "pattern" "expression"
       | IfCondition =>
           desc "if condition is not boolean" "condition" "required"
       | IfBranches =>
           desc "types of if branches do not agree" "then branch" "else branch"
       | WhileCondition =>
           desc "while condition is not boolean" "condition" "required"
       | AndalsoOperand =>
           desc "operand of andalso is not boolean" "operand" "required"
       | OrelseOperand =>
           desc "operand of orelse is not boolean" "operand" "required"
       | MatchRules =>
           desc "types of rules do not agree" "expected" "this rule"
       | CaseObject =>
           desc "case object and rules do not agree" "rule domain" "object"
       | HandlerDomain =>
           desc "handler domain is not exn" "handler domain" "required"
       | HandlerResult =>
           desc "expression and handler do not agree" "body" "handler range"
       | RaiseArgument =>
           desc "argument of raise is not an exception" "raised" "required"
       | ConstructorArgument =>
           desc "constructor and argument do not agree in pattern"
             "expected" "argument"
       | FunctionConstraint =>
           desc "type constraint of val rec declaration is not a function type"
             "inferred type" "constraint"
       | FunctionResult =>
           desc "right-hand side of function clause does not agree with function result type"
             "expression" "result type"
       | FunctionClauses =>
           desc "parameter or result constraints of clauses do not agree"
             "this clause" "expected"
       | DoExpression =>
           desc "do expression does not have type unit" "required" "expression"
       | VectorElements =>
           desc "vector elements do not have the same type"
             "earlier element" "this element"
       | VectorPatternElements =>
           desc "vector-pattern elements do not have the same type"
             "earlier element" "this element"

  (* Source context *)

  fun emitSite env stream site =
    let val depth = !Control_Print.printDepth
        fun heading s = (PP.newline stream; PP.string stream ("in " ^ s ^ ":"))
    in  case site
          of NoSite => ()
           | AtExpression exp =>
               (heading "expression"; PP.break stream {nsp=1, offset=2};
                PPAbsyn.ppExp (env, NONE) stream (exp, depth))
           | AtPattern pat =>
               (heading "pattern"; PP.break stream {nsp=1, offset=2};
                PPAbsyn.ppPat env stream (pat, depth))
           | AtRule rule =>
               (heading "rule"; PP.break stream {nsp=1, offset=2};
                PPAbsyn.ppRule (env, NONE) stream (rule, depth))
           | AtValueBinding vb =>
               (heading "declaration"; PP.break stream {nsp=1, offset=2};
                PPAbsyn.ppVB (env, NONE) stream (vb, depth))
           | AtRecursiveBinding rvb =>
               (heading "declaration"; PP.break stream {nsp=1, offset=2};
                PPAbsyn.ppRVB (env, NONE) stream (rvb, depth))
           | AtDeclaration dec =>
               (heading "declaration"; PP.break stream {nsp=1, offset=2};
                PPAbsyn.ppDec (env, NONE) stream (dec, depth))
    end

  fun firstFailure triple =
    let fun search [] = NONE
          | search (Unify.ERR triple :: _) = SOME (walk triple)
          | search (_ :: rest) = search rest
        and walk (triple as (_, _, Unify.CTX {args, ...})) =
              (case search args of SOME x => x | NONE => triple)
          | walk triple = triple
    in  walk triple
    end

  fun labelNames labels = String.concatWith ", " (map S.name labels)

  fun plural (singular, plural) n =
    if n = 1 then singular else plural

  fun count n nouns =
    concat [Int.toString n, " ", plural nouns n]

  fun named labels nouns =
    concat [plural nouns (length labels), " ", labelNames labels]

  fun arrowFailureIndex failure =
    case failure
      of Unify.CTX {tycon, args} =>
           let fun find (_, []) = NONE
                 | find (i, Unify.ERR _ :: _) = SOME i
                 | find (i, _ :: rest) = find (i + 1, rest)
           in  if isArrowTyc tycon then find (0, args) else NONE
           end
       | _ => NONE

  (* Help messages *)
  fun contextHelp (kind, failure) =
    case kind
      of IfCondition =>
           SOME "use a comparison or another expression of type bool as the condition"
       | WhileCondition =>
           SOME "use a comparison or another expression of type bool as the condition"
       | IfBranches =>
           SOME "convert one branch so that both branches return the same type"
       | MatchRules =>
           (case arrowFailureIndex failure
              of SOME 0 => SOME "every rule pattern must accept the same input type"
               | SOME 1 => SOME "every rule in a match must return the same type"
               | _ => SOME "all rules must have compatible pattern and result types")
       | CaseObject =>
           SOME "change the case expression or its patterns so that their types agree"
       | HandlerDomain =>
           SOME "match an exception constructor instead of a value of this type"
       | HandlerResult =>
           SOME "change the handler result or body so that both return the same type"
       | RaiseArgument =>
           SOME "raise expects an exception value, usually created by an exception constructor"
       | DoExpression =>
           SOME "discard an unwanted result with ignore, for example do ignore (expression)"
       | VectorElements =>
           SOME "convert the differing element or wrap alternatives in one datatype"
       | VectorPatternElements =>
           SOME "use patterns for one element type throughout the vector pattern"
       | FunctionConstraint =>
           SOME "remove the constraint or give the recursive function an arrow type"
       | _ => NONE

  fun isReal ty = TU.equalType (TU.prune ty, BT.realTy)

  fun isFunction ty =
    case TU.headReduceType (TU.prune ty)
      of T.CONty (tycon, _) => isArrowTyc tycon
       | _ => false

  fun isAbstract ty =
    case TU.headReduceType (TU.prune ty)
      of T.CONty (T.GENtyc {kind, eq, ...}, _) =>
           (case !eq
              of T.ABS => true
               | _ =>
                   (case kind
                      of T.ABSTRACT _ => true
                       | T.FLEXTYC _ => true
                       | T.FORMAL => true
                       | _ => false))
       | _ => false

  fun isNum ty =
    case TU.prune ty
      of T.VARty (ref (T.OVLDI _ | T.OVLDW _)) => true
       | ty => TU.equalType (ty, BT.intTy)

  fun numericHelp (left, right) =
    if (isNum left andalso isReal right)
       orelse (isReal left andalso isNum right)
    then SOME "use operands of the same numeric type"
    else NONE

  fun recordHelp (leftName, rightName, leftLabels, rightLabels) =
    let val leftOnly =
          List.filter (fn label => not (hasLabel label rightLabels)) leftLabels
        val rightOnly =
          List.filter (fn label => not (hasLabel label leftLabels)) rightLabels
        val details =
          (if null leftOnly then []
           else [leftName ^ " has " ^ named leftOnly ("field", "fields")]) @
          (if null rightOnly then []
           else [rightName ^ " has " ^ named rightOnly ("field", "fields")])
    in  if null details
          then NONE
          else SOME (String.concatWith "; " details)
    end

  fun failureHelp (env, leftName, rightName, left, right, failure) =
    case failure
      of Unify.TYC (leftTyc as T.RECORDtyc leftLabels,
                    rightTyc as T.RECORDtyc rightLabels, _, _) =>
           if Tuples.isTUPLEtyc leftTyc andalso Tuples.isTUPLEtyc rightTyc
             then SOME (concat [
                 "tuple lengths must match; ", leftName, " has ",
                 count (length leftLabels) ("element", "elements"), " and ",
                 rightName, " has ",
                 count (length rightLabels) ("element", "elements")
               ])
             else recordHelp (leftName, rightName, leftLabels, rightLabels)
       | Unify.CIRC _ => SOME
           "this use would require an infinite type; check whether a value is \
           \being applied to itself"
       | Unify.EQ =>
           if isReal right then
             SOME "values of type real cannot be compared with =; use Real.== instead"
           else if isFunction right then
             SOME "functions cannot be compared for equality"
           else if isAbstract right then
             SOME ("the type " ^ typeToString env 1000 right ^ " is abstract here")
           else
             SOME "operator = requires an equality type"
       | _ => NONE

  fun helpText (env, kind, leftName, rightName, left, right, failure) =
    let val (leafLeft, leafRight, leafFailure) =
          firstFailure (left, right, failure)
    in  case failureHelp
               (env, leftName, rightName, leafLeft, leafRight, leafFailure)
          of SOME help => SOME help
           | NONE =>
               (case numericHelp (leafLeft, leafRight)
                  of SOME help => SOME help
                   | NONE => contextHelp (kind, failure))
    end

  fun emitHelp stream help =
    emitDoc Unstyled stream
      ("help: ", join (soft " ") (map text (String.tokens Char.isSpace help)))

  fun focusApplication (kind, left, right, failure) =
    case (kind, failure)
      of (ApplicationArgument, Unify.CTX {tycon, args=Unify.ERR triple :: _}) =>
           if isArrowTyc tycon then triple else (left, right, failure)
       | (ConstructorArgument, Unify.CTX {tycon, args=Unify.ERR triple :: _}) =>
           if isArrowTyc tycon then triple else (left, right, failure)
       | _ => (left, right, failure)

  (* Diagnostic output *)

  fun emitComparison env stream (kind, leftTy, rightTy, failure) =
    let val (leftTy, rightTy, failure) =
          focusApplication (kind, leftTy, rightTy, failure)
        val {leftLabel, rightLabel, ...} = describeMismatch kind
        val left = renderTy Left env (leftTy, rightTy, failure)
        val right = renderTy Right env (leftTy, rightTy, failure)
        val mode = if PP.color stream then Color else Marks
        val labelWidth = Int.max (size leftLabel, size rightLabel)
        fun prefix label =
          StringCvt.padRight #" " (labelWidth + 2) (label ^ ": ")
    in  emitDoc mode stream (prefix leftLabel, left);
        PP.newline stream;
        emitDoc mode stream (prefix rightLabel, right);
        case helpText
               (env, kind, leftLabel, rightLabel, leftTy, rightTy, failure)
          of NONE => ()
           | SOME help => (PP.newline stream; emitHelp stream help)
    end

  fun mismatchHeadline (kind, left, right, failure) =
    let val (_, _, leafFailure) = firstFailure (left, right, failure)
    in  case leafFailure
          of Unify.TYC (tc1 as T.RECORDtyc _, tc2 as T.RECORDtyc _, _, _) =>
               if Tuples.isTUPLEtyc tc1 andalso Tuples.isTUPLEtyc tc2
                 then "tuple length mismatch"
                 else "record field mismatch"
           | _ => #headline (describeMismatch kind)
    end

  fun emitLine stream contents = (
      PP.newline stream;
      PP.string stream contents
    )

  fun emitWithSite env stream site body = (
      emitSite env stream site;
      PP.newline stream;
      body ()
    )

  fun emitKnownFields _ [] = ()
    | emitKnownFields stream fields = (
        PP.string stream (concat [
          plural ("known field: ", "known fields: ") (length fields),
          labelNames fields
        ]);
        PP.newline stream
      )

  fun report (env, err, diagnostic) =
    let fun emit region headline body = err region ErrorMsg.COMPLAIN headline body
    in  PPType.resetPPType();
        case diagnostic
          of Mismatch {kind, left, right, failure, site, region} =>
               let val headline = mismatchHeadline (kind, left, right, failure)
               in emit region headline (fn stream =>
                    emitWithSite env stream site (fn () =>
                      emitComparison env stream (kind, left, right, failure)))
               end
           | NotFunction {operatorTy, site, region} =>
               emit region "operator is not a function" (fn stream =>
                 emitWithSite env stream site (fn () =>
                   (PP.string stream "operator type: ";
                    PP.string stream
                      (typeToString env (!Control_Print.lineWidth) operatorTy))))
           | MissingRecordField {label, recordTy, site, region} =>
               emit region "selecting a non-existing field from a record" (fn stream =>
                 (emitLine stream ("field: " ^ S.name label);
                  emitLine stream "record type: ";
                  PPType.ppType env stream recordTy;
                  emitSite env stream site;
                  PP.newline stream;
                  emitHelp stream
                    "check the field name or add it to the record type"))
           | UnresolvedFlexRecord {knownFields, region, ...} =>
               emit region "cannot determine the complete record type" (fn stream =>
                 (PP.newline stream;
                  emitKnownFields stream knownFields;
                  emitHelp stream
                    "add a type annotation specifying all record fields"))
           | ExplicitTyvarGeneralization {tyvar, region} =>
               emit region "explicit type variable cannot be generalized at its binding declaration"
                 (fn stream =>
                   (emitLine stream
                      ("type variable: " ^ PPType.tyvarPrintname tyvar);
                    PP.newline stream;
                    emitHelp stream
                      "the value restriction prevents generalization here; \
                      \remove the explicit type variable or make the \
                      \right-hand side a syntactic function"))
           | TopLevelExceptionTyvar {region} =>
               emit region "type variable in top-level exception type" (fn stream =>
                 (PP.newline stream;
                  emitHelp stream
                    "top-level exception argument types cannot be polymorphic"))
    end

end (* structure Diagnostic *)
