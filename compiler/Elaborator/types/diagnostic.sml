(* diagnostic.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (http://www.smlnj.org)
 *
 * Print diagnostic messages when a type error occurs.
 *)

structure Diagnostic :> sig

  (* Print the diagnostic. *)
  val pp :
    StaticEnv.staticEnv -> PrettyPrint.stream -> Types.ty * Types.ty * Unify.unifyFail  -> unit
end = struct

  structure BT = BasicTypes
  structure PP = PrettyPrint
  structure S  = Symbol
  structure T  = Types
  structure TU = TypesUtil

  val useColor = true

  (* A tiny pretty-printer document for rendering type fragments.
   *
   * Text(false, s) is ordinary text, Text(true, s) is text that should be
   * highlighted, and Soft s is ordinary text that may start a continuation line
   * when it would exceed Control_Print.lineWidth. *)
  datatype piece
    = Text of bool * string
    | Soft of string

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

  fun text s : doc = [Text(false, s)]
  fun mark s : doc = [Text(true, s)]
  fun soft s : doc = [Soft s]

  fun join _ [] = []
    | join _ [x] = x
    | join sep (x :: xs) = x @ sep @ join sep xs

  fun typeString env width ty =
    let val ty =
          case TU.prune ty
            of T.VARty (ref (T.OVLDI _)) => List.nth (OverloadClasses.intClass, 0)
             | T.VARty (ref (T.OVLDW _)) => List.nth (OverloadClasses.wordClass, 0)
             | _ => ty
    in  PP.pp_to_string_sans width (PPType.ppType env) ty
    end

  fun tyconString env width tycon =
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

  fun hasLabel label = List.exists (fn label' => Symbol.eq (label, label'))

  local
    val AnsiHL = ANSITerm.toString [ANSITerm.FG ANSITerm.Red]
    val AnsiReset = ANSITerm.toString []
  in
    fun color s = concat [AnsiHL, s, AnsiReset]
  end

  (* Convert a document into concrete text lines and matching highlighted lines.
   *)
  fun renderLines width prefix doc =
    let val prefixLen = size prefix
        val initMarks = spaces prefixLen
        val textLine = ref prefix
        val markLine = ref initMarks
        val col = ref prefixLen
        val lines = ref ([] : (string * string) list)

        fun emitLine () = (
          lines := (!textLine, !markLine) :: !lines;
          textLine := initMarks;
          markLine := initMarks;
          col := prefixLen
        )

        fun emitStringColor isMarked s = (
          textLine := !textLine ^ (if isMarked then color s else s);
          (* markLine does not change *)
          col := !col + size s
        )

        fun emitStringPlain isMarked s = (
          textLine := !textLine ^ s;
          markLine := !markLine ^
            (if isMarked
              then String.map markedChar s
              else String.map (fn _ => #" ") s);
          col := !col + size s
        )

        val emitString =
          if useColor then emitStringColor else emitStringPlain

        fun emitPiece (Text(marked, s)) = emitString marked s
          | emitPiece (Soft s) =
              if !col + size s > width andalso !col > prefixLen then
                emitLine ()
              else
                emitString false s
    in  app emitPiece doc;
        rev ((!textLine, !markLine) :: !lines)
    end

  fun ppRendered ppstrm (prefix, doc) =
    let val width = !Control_Print.lineWidth
        fun ppOne (line, marks) = (
          PP.string ppstrm line;
          PP.newline ppstrm;
          if anyMarked marks then
            (PP.string ppstrm marks; PP.newline ppstrm)
          else
            ()
        )
    in  app ppOne (renderLines width prefix doc)
    end

  datatype side = Left | Right
  fun select Left (x, _) = x
    | select Right (_, y) = y

  (* Render one side of a unification failure. *)
  fun renderTy side env (ty1, ty2, failure) =
    let val width = !Control_Print.lineWidth
        fun typeDoc ty = mark (typeString env width ty)
        fun tyconDoc tycon = mark (tyconString env width tycon)

        val select = fn x => select side x

        (* OK and UNK are both irrelevant for diagnostic, so both become "_". *)
        fun renderArg (Unify.OK ty) = text (typeString env width ty)
          | renderArg (Unify.UNK _) = text "_"
          | renderArg (Unify.ERR failure) = render failure

        and argNeedsParen (Unify.ERR (ty1, ty2, failure)) =
              tyNeedsParen (select (ty1, ty2))
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
                text (Symbol.name label ^ ":") @ renderArg arg
              val fields = ListPair.mapEq field (labels, args)
          in  text "{" @ join (text "," @ soft " ") fields @ text "}"
          end

        and renderArrow [domain, range] =
              renderArgParen domain @ soft " " @ text "-> "
                                         @ renderArg range
          | renderArrow _ = bug "arrow tycon with wrong arity"

        and renderTypeArgs [] = []
          | renderTypeArgs [arg] = renderArgParen arg @ soft " "
          | renderTypeArgs args =
              text "(" @ join (text "," @ soft " ") (map renderArg args)
                       @ text ")" @ soft " "

        and renderCTX {tycon, args} =
          if Tuples.isTUPLEtyc tycon then
            renderTuple args
          else if isArrowTyc tycon then
            renderArrow args
          else
            (case tycon
               of T.RECORDtyc labels => renderRecord (labels, args)
                | _ =>
                    renderTypeArgs args @ text (tyconString env width tycon))

        and renderRecordTyc (labels, otherLabels) =
          let fun differs label = not (hasLabel label otherLabels)
              fun field label =
                let val l =
                      if differs label then
                        mark (Symbol.name label)
                      else
                        text (Symbol.name label)
                in  l @ text ":_"
                end
          in  if Tuples.isTUPLEtyc (T.RECORDtyc labels) then
                join (soft " " @ text "* ")
                  (map (fn label => if differs label then mark "_" else text "_") labels)
              else
                text "{" @ join (text "," @ soft " ") (map field labels) @ text "}"
          end

        and render (ty1, ty2, failure) =
          case failure
            of Unify.CTX ctx => renderCTX ctx
             | Unify.TYC (T.RECORDtyc labels1, T.RECORDtyc labels2, _, _) =>
                 renderRecordTyc (select ((labels1, labels2), (labels2, labels1)))
             | _ => typeDoc (select (ty1, ty2))
    in  render (ty1, ty2, failure)
    end

  fun pp env ppstrm failure =
    (PPType.resetPPType();
     let val left = renderTy Left env failure
         val right = renderTy Right env failure
     in  PP.newline ppstrm;
         ppRendered ppstrm ("expected: ", left);
         ppRendered ppstrm ("found:    ", right)
     end)

end (* structure Diagnostic *)
