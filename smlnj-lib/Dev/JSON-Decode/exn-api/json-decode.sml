(* json-decode.sml
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure JSONDecode :> JSON_DECODE =
  struct

    structure U = JSONUtil

    datatype value = datatype JSON.value

    exception Failure of string * JSON.value
    exception NotNull of JSON.value
    exception NotBool = JSONUtil.NotBool
    exception NotInt = JSONUtil.NotInt
    exception NotNumber = JSONUtil.NotNumber
    exception NotString = JSONUtil.NotString
    exception NotObject = JSONUtil.NotObject
    exception FieldNotFound = JSONUtil.FieldNotFound
    exception NotArray = JSONUtil.NotArray
    exception ArrayBounds = JSONUtil.ArrayBounds

    datatype 'a decoder = D of value -> 'a

    fun decode (D d) jv = d jv

    fun decodeString decoder s =
          (decode decoder (JSONParser.parse(JSONParser.openString s)))

    fun decodeFile decoder fname =
          (decode decoder (JSONParser.parseFile fname))

    fun asBool (BOOL b) = b
      | asBool v = raise NotBool v
    val bool = D asBool

    fun asInt jv = (case jv
           of INT n => Int.fromLarge n
            | INTLIT n => (case Int.fromString n
                 of SOME n => n
                  | NONE => raise NotInt jv (* should be impossible *)
                (* end case *))
            | _ => raise NotInt jv
          (* end case *))
    val int = D asInt

    fun asIntInf (INT n) = n
      | asIntInf (v as INTLIT n) = (case IntInf.fromString n
           of SOME n => n
            | NONE => raise NotInt v (* should be impossible *)
          (* end case *))
      | asIntInf v = raise NotInt v
    val intInf = D asIntInf

    fun asNumber (INT n) = Real.fromLargeInt n
      | asNumber (v as INTLIT n) = (case IntInf.fromString n
           of SOME n => Real.fromLargeInt n
            | NONE => raise NotNumber v (* should be impossible *)
          (* end case *))
      | asNumber (FLOAT f) = f
      | asNumber v = raise NotNumber v
    val number = D asNumber

    fun asString (STRING s) = s
      | asString v = raise NotString v
    val string = D asString

    fun null dflt = D(fn NULL => dflt | jv => raise NotNull jv)

    val raw = D(fn jv => jv)

    fun nullable (D decoder) = let
          fun decoder' NULL = NONE
            | decoder' jv = SOME(decoder jv)
          in
            D decoder'
          end

    fun array (D elemDecoder) = let
          fun decodeList ([], elems) = List.rev elems
            | decodeList (jv::jvs, elems) = decodeList(jvs, elemDecoder jv :: elems)
          fun decoder (ARRAY elems) = decodeList (elems, [])
            | decoder jv = raise NotArray jv
          in
            D decoder
          end

    fun try (D d) = D(fn jv => (SOME(d jv) handle _ => NONE))

    fun seq (D d1) (D d2) = D(fn jv => let
          val v = d1 jv
          val k = d2 jv
          in
            k v
          end)

    fun field key valueDecoder = D(fn jv => (case jv
           of OBJECT fields => (case List.find (fn (l, v) => (l = key)) fields
                 of SOME(_, v) => decode valueDecoder v
                  | _ => raise FieldNotFound(
                      jv,
                      concat["no definition for field \"", key, "\""])
                (* end case *))
            | _ => raise NotObject jv
          (* end case *)))

    fun reqField key valueDecoder k = seq (field key valueDecoder) k

    fun optField key (D valueDecoder) (D objDecoder) = let
          fun objDecoder' optFld jv = (objDecoder jv) optFld
          fun decoder jv = (case U.findField jv key
                 of SOME NULL => objDecoder' NONE jv
                  | SOME jv' => objDecoder' (SOME(valueDecoder jv')) jv
                  | NONE => objDecoder' NONE jv
                (* end case *))
          in
            D decoder
          end

    fun dfltField key (D valueDecoder) dfltVal (D objDecoder) = let
          fun objDecoder' fld jv = (objDecoder jv) fld
          fun decoder jv = (case U.findField jv key
                 of SOME NULL => objDecoder' dfltVal jv
                  | SOME jv' => objDecoder' (valueDecoder jv') jv
                  | NONE => objDecoder' dfltVal jv
                (* end case *))
          in
            D decoder
          end

    fun sub i (D d) = D(fn jv => (case jv
           of jv as ARRAY arr => let
                fun get (0, item::_) = d item
                  | get (_, []) = raise ArrayBounds(jv, i)
                  | get (i, _::r) = get (i-1, r)
                in
                  if (i < 0) then ArrayBounds(jv, i) else get (i, arr)
                end
            | _ => raise NotArray jv
          (* end case *)))

    fun at path (D d) = D(fn jv => d (U.get(jv, path)))

    fun succeed x = D(fn _ => x)

    fun fail msg = D(fn jv => raise Failure(msg, jv))

    fun andThen f (D d) = D(fn jv => decode (f (d jv)) jv)

    fun orElse (D d1, D d2) = D(fn jv => (d1 jv handle _ => d2 jv))

    (* `choose [d1, ..., dn]` is equivalent to
     * `orElse(d1, orElse(d2, ..., orElse(dn, fail "no choice") ... ))`
     *)
    fun choose [] = fail "no choice"
      | choose (d::ds) = orElse(d, choose ds)

    fun map f (D decoder) = D(fn jv => f (decoder jv))

    fun map2 f (D d1, D d2) = D(fn jv => f(d1 jv, d2 jv))
    fun map3 f (D d1, D d2, D d3) = D(fn jv => f(d1 jv, d2 jv, d3 jv))
    fun map4 f (D d1, D d2, D d3, D d4) = D(fn jv => f(d1 jv, d2 jv, d3 jv, d4 jv))

    fun tuple2 (d1, d2) = map2 Fn.id (d1, d2)
    fun tuple3 (d1, d2, d3) = map3 Fn.id (d1, d2, d3)
    fun tuple4 (d1, d2, d3, d4) = map4 Fn.id (d1, d2, d3, d4)

    fun delay dd = andThen dd (succeed ())

  end
