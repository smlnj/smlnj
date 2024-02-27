(* json-decode.sml
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure JSONDecode :> JSON_DECODE =
  struct

    structure U = JSONUtil

    datatype value = datatype JSON.value

    datatype 'a result = Err of exn | Ok of 'a

    exception Failure of string * JSON.value
    exception NotNull of JSON.value
    exception NotBool = JSONUtil.NotBool
    exception NotInt = JSONUtil.NotInt
    exception NotNumber = JSONUtil.NotNumber
    exception NotString = JSONUtil.NotString
    exception NotObject = JSONUtil.NotObject
    exception FieldNotFound = JSONUtil.FieldNotFound
    exception NotArray = JSONUtil.NotArray

    datatype 'a decoder = D of value -> 'a result

    fun decode (D f) jv = f jv

    fun decodeString decoder s =
          (decode decoder (JSONParser.parse(JSONParser.openString s)))
            handle exn => Err exn

    fun asBool (BOOL b) = Ok b
      | asBool v = Err(NotBool v)
    val bool = D asBool

    fun asInt jv = (case jv
           of INT n => Ok(Int.fromLarge n)
            | INTLIT n => (case Int.fromString n
                 of SOME n => Ok n
                  | NONE => Err(NotInt jv) (* should be impossible *)
                (* end case *))
            | _ => Err(NotInt jv)
          (* end case *))
            handle ex => Err ex (* for Overflow *)
    val int = D asInt

    fun asIntInf (INT n) = Ok n
      | asIntInf (v as INTLIT n) = (case IntInf.fromString n
           of SOME n => Ok n
            | NONE => Err(NotInt v) (* should be impossible *)
          (* end case *))
      | asIntInf v = Err(NotInt v)
    val intInf = D asIntInf

    fun asNumber (INT n) = Ok(Real.fromLargeInt n)
      | asNumber (v as INTLIT n) = (case IntInf.fromString n
           of SOME n => Ok(Real.fromLargeInt n)
            | NONE => Err(NotNumber v) (* should be impossible *)
          (* end case *))
      | asNumber (FLOAT f) = Ok f
      | asNumber v = Err(NotNumber v)
    val number = D asNumber

    fun asString (STRING s) = Ok s
      | asString v = Err(NotString v)
    val string = D asString

    fun null dflt = D(fn NULL => Ok dflt | jv => Err(NotNull jv))

    val raw = D(fn jv => Ok jv)

    fun nullable (D decoder) = let
          fun decoder' NULL = Ok NONE
            | decoder' jv = (case decoder jv
                 of Ok x => Ok(SOME x)
                  | Err ex => Err ex
                (* end case *))
          in
            D decoder'
          end

    fun list (D elemDecoder) = let
          fun decodeList ([], elems) = Ok(List.rev elems)
            | decodeList (jv::jvs, elems) = (case elemDecoder jv
                 of Ok elem => decodeList(jvs, elem::elems)
                  | Err ex => Err ex
                (* end case *))
          fun decoder (ARRAY elems) = decodeList (elems, [])
            | decoder jv = Err(JSONUtil.NotArray jv)
          in
            D decoder
          end

    fun try (D d) = D(fn jv => (case d jv
           of Err _ => Ok NONE
            | Ok x => Ok(SOME x)
          (* end case *)))

    fun field key valueDecoder = D(fn jv => (case jv
           of OBJECT fields => (case List.find (fn (l, v) => (l = key)) fields
                 of SOME(_, v) => decode valueDecoder v
                  | _ => Err(FieldNotFound(
                      jv,
                      concat["no definition for field \"", key, "\""]))
                (* end case *))
            | _ => Err(NotObject jv)
          (* end case *)))

    fun reqField key (D valueDecoder) (D objDecoder) = let
          fun decoder jv = (case valueDecoder (U.lookupField jv key)
                 of Ok fld => (case objDecoder jv
                       of Ok mkObj => Ok(mkObj fld)
                        | Err ex => Err ex
                      (* end case *))
                  | Err ex => Err ex
                (* end case *))
                  handle ex => Err ex
          in
            D decoder
          end

    fun optField key (D valueDecoder) (D objDecoder) = let
          fun objDecoder' optFld jv = (case objDecoder jv
                 of Ok mkObj => Ok(mkObj optFld)
                  | Err ex => Err ex
                (* end case *))
          fun decoder jv = (case U.findField jv key
                 of SOME NULL => objDecoder' NONE jv
                  | SOME jv' => (case valueDecoder jv'
                       of Ok fld => objDecoder' (SOME fld) jv
                        | Err ex => Err ex
                      (* end case *))
                  | NONE => objDecoder' NONE jv
                (* end case *))
                  handle ex => Err ex
          in
            D decoder
          end

    fun dfltField key (D valueDecoder) dfltVal (D objDecoder) = let
          fun objDecoder' fld jv = (case objDecoder jv
                 of Ok mkObj => Ok(mkObj fld)
                  | Err ex => Err ex
                (* end case *))
          fun decoder jv = (case U.findField jv key
                 of SOME NULL => objDecoder' dfltVal jv
                  | SOME jv' => (case valueDecoder jv'
                       of Ok fld => objDecoder' fld jv
                        | Err ex => Err ex
                      (* end case *))
                  | NONE => objDecoder' dfltVal jv
                (* end case *))
                  handle ex => Err ex
          in
            D decoder
          end

    fun sub i (D d) = D(fn jv => (case jv
           of ARRAY arr => (d (List.nth(arr, i)) handle ex => Err ex)
            | _ => Err(NotArray jv)
          (* end case *)))

    fun at path (D d) = D(fn jv => (d (U.get(jv, path)) handle ex => Err ex))

    fun succeed x = D(fn _ => Ok x)

    fun fail msg = D(fn jv => Err(Failure(msg, jv)))

    fun andThen f (D d) = let
          fun decoder jv = (case d jv
                 of Ok x => decode (f x) jv
                  | Err ex => Err ex
                (* end case *))
          in
            D decoder
          end

    fun orElse (D d1, D d2) = D(fn jv => (case d1 jv
           of Err _ => d2 jv
            | ok => ok
          (* end case *)))

    (* `choose [d1, ..., dn]` is equivalent to
     * `orElse(d1, orElse(d2, ..., orElse(dn, fail "no choice") ... ))`
     *)
    fun choose [] = fail "no choice"
      | choose (d::ds) = orElse(d, choose ds)

    fun map f (D decoder) = let
          fun mapf jv = (case decoder jv
                 of Ok v => (Ok(f v) handle ex => Err ex)
                  | Err ex => Err ex
                (* end case *))
          in
            D mapf
          end

    fun map2 f (D d1, D d2) = D(fn jv => (case d1 jv
           of Ok v1 => (case d2 jv
                 of Ok v2 => (Ok(f (v1, v2)) handle ex => Err ex)
                  | Err ex => Err ex
                (* end case *))
            | Err ex => Err ex
          (* end case *)))

    fun map3 f (D d1, D d2, D d3) = D(fn jv => (case d1 jv
           of Ok v1 => (case d2 jv
                 of Ok v2 => (case d3 jv
                       of Ok v3 => (Ok(f (v1, v2, v3)) handle ex => Err ex)
                        | Err ex => Err ex
                      (* end case *))
                  | Err ex => Err ex
                (* end case *))
            | Err ex => Err ex
          (* end case *)))

  end
