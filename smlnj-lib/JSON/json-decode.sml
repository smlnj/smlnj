(* json-decode.sml
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure JSONDecode :> sig

    (* exceptions used as errors; note that most of these come from the
     * JSONUtil module.
     *)
    exception Failure of string * JSON.value
    exception NotNull of JSON.value
    exception NotBool of JSON.value
    exception NotInt of JSON.value
    exception NotNumber of JSON.value
    exception NotString of JSON.value
    exception NotObject of JSON.value
    exception FieldNotFound of JSON.value * string
    exception NotArray of JSON.value
    exception ArrayBounds of JSON.value * int
    exception ElemNotFound of JSON.value

    val exnMessage : exn -> string

    type 'a decoder

    val decode : 'a decoder -> JSON.value -> 'a
    val decodeString : 'a decoder -> string -> 'a
    val decodeFile : 'a decoder -> string -> 'a

    val bool : bool decoder
    val int : int decoder
    val intInf : IntInf.int decoder
    val number : Real64.real decoder
    val string : string decoder

    val null : 'a -> 'a decoder

    (* returns the raw JSON value without further decoding *)
    val raw : JSON.value decoder

    (* returns a decoder that maps the JSON `null` value to `NONE` and otherwise
     * returns `SOME v`, where `v` is the result of decoding the value using
     * the supplied decoder.
     *)
    val nullable : 'a decoder -> 'a option decoder

    (* returns a decoder that attempts to decode a value and returns `NONE`
     * on failure (instead of an error result).
     *)
    val try : 'a decoder -> 'a option decoder

    (* sequence decoders using "continuation-passing" style; for example
     *
     *  seq (field "x" number)
     *      (succeed (fn x => x*x))
     *)
    val seq : 'a decoder -> ('a -> 'b) decoder -> 'b decoder

    (* `field key d` returns a decoder that decodes the specified object field
     * using the decoder `d`.
     *)
    val field : string -> 'a decoder -> 'a decoder

    (* decode a required field *)
    val reqField : string -> 'a decoder -> ('a -> 'b) decoder -> 'b decoder

    (* decode an optional field *)
    val optField : string -> 'a decoder -> ('a option -> 'b) decoder -> 'b decoder

    (* decode an optional field that has a default value *)
    val dfltField : string -> 'a decoder -> 'a -> ('a -> 'b) decoder -> 'b decoder

    (* decodes a JSON ARRAY into a list of values *)
    val array : 'a decoder -> 'a list decoder

    (* `sub i d` returns a decoder that when given a JSON array, decodes the i'th
     * array element.
     *)
    val sub : int -> 'a decoder -> 'a decoder

    (* returns a decoder that decodes the value at the location specified by
     * the path.
     *)
    val at : JSONUtil.path -> 'a decoder -> 'a decoder

    (* `succeed v` returns a decoder that always yields `v` for any JSON input *)
    val succeed : 'a -> 'a decoder

    (* `fail msg` returns a decoder that raises `Failure(msg, jv)` for
     * any JSON input `jv`.
     *)
    val fail : string -> 'a decoder

    val andThen : ('a -> 'b decoder) -> 'a decoder -> 'b decoder

    (* `orElse (d1, d2)` returns a decoder that first trys `d1` and returns its
     * result if it succeeds.  If `d1` fails, then it returns the result of trying
     * `d2`.
     *)
    val orElse : 'a decoder * 'a decoder -> 'a decoder

    (* `choose [d1, ..., dn]` is equivalent to
     * `orElse(d1, orElse(d2, ..., orElse(dn, fail "no choice") ... ))`
     *)
    val choose : 'a decoder list -> 'a decoder

    val map : ('a -> 'b) -> 'a decoder -> 'b decoder
    val map2 : ('a * 'b -> 'res)
          -> ('a decoder * 'b decoder)
          -> 'res decoder
    val map3 : ('a * 'b * 'c -> 'res)
          -> ('a decoder * 'b decoder * 'c decoder)
          -> 'res decoder
    val map4 : ('a * 'b * 'c * 'd -> 'res)
          -> ('a decoder * 'b decoder * 'c decoder * 'd decoder)
          -> 'res decoder

    (* versions of the map combinators that just apply the identity to the tuple *)
    val tuple2 : ('a decoder * 'b decoder) -> ('a * 'b) decoder
    val tuple3 : ('a decoder * 'b decoder * 'c decoder) -> ('a * 'b * 'c) decoder
    val tuple4 : ('a decoder * 'b decoder * 'c decoder * 'd decoder)
          -> ('a * 'b * 'c * 'd) decoder

    (* a delay combinator for defining recursive decoders *)
    val delay : (unit -> 'a decoder) -> 'a decoder

  end = struct

    structure U = JSONUtil

    (* import the error exceptions and exnMessage *)
    open Errors

    datatype value = datatype JSON.value

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
            | _ => raise NotInt jv
          (* end case *))
    val int = D asInt

    fun asIntInf (INT n) = n
      | asIntInf v = raise NotInt v
    val intInf = D asIntInf

    fun asNumber (INT n) = Real.fromLargeInt n
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
                  | _ => raise FieldNotFound(jv, key)
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
                  if (i < 0) then raise ArrayBounds(jv, i) else get (i, arr)
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
