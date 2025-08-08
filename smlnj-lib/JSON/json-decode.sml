(* json-decode.sml
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure JSONDecode :> sig

    (* exceptions used as errors; note that most of these come from the
     * JSONUtil module.  The standard practice is to raise `JSONError(ex, v)`
     * for an error on JSON value `v`, where `ex` specifies more detail about
     * the actual error.
     *)
    exception JSONError of exn * JSON.value

    (* specific errors that are used as the first argument to `JSONError` *)
    exception NotNull
    exception NotBool
    exception NotInt
    exception NotNumber
    exception NotString
    exception NotObject
    exception FieldNotFound of string
    exception NotArray
    exception ArrayBounds of int
    exception ElemNotFound

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

    (* decides a JSON OBJECT into a list of labeled JSON values *)
    val rawObject : (string * JSON.value) list decoder

    (* decides a JSON ARRAY into a vector of JSON values *)
    val rawArray : JSON.value vector decoder

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

    (* `fail msg` returns a decoder that raises `JSONError(Fail msg, jv)` for
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
      | asBool v = notBool v
    val bool = D asBool

    fun asInt jv = (case jv
           of INT n =>
                (Int.fromLarge n handle Overflow => raise (JSONError(Overflow, jv)))
            | _ => notInt jv
          (* end case *))
    val int = D asInt

    fun asIntInf (INT n) = n
      | asIntInf v = notInt v
    val intInf = D asIntInf

    fun asNumber (INT n) = Real.fromLargeInt n
      | asNumber (FLOAT f) = f
      | asNumber v = notNumber v
    val number = D asNumber

    fun asString (STRING s) = s
      | asString v = notString v
    val string = D asString

    fun null dflt = D(fn NULL => dflt | jv => notNull jv)

    val raw = D(fn jv => jv)

    val rawObject = D(fn (OBJECT fields) => fields | jv => notObject jv)

    val rawArray = D(fn (ARRAY elems) => Vector.fromList elems | jv => notArray jv)

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
            | decoder jv = notArray jv
          in
            D decoder
          end

    fun try (D d) = D(fn jv => (SOME(d jv) handle JSONError _ => NONE | ex => raise ex))

    fun seq (D d1) (D d2) = D(fn jv => let
          val v = d1 jv
          val k = d2 jv
          in
            k v
          end)

    fun field key valueDecoder = D(fn jv => (case jv
           of OBJECT fields => (case List.find (fn (l, v) => (l = key)) fields
                 of SOME(_, v) => decode valueDecoder v
                  | _ => fieldNotFound(key, jv)
                (* end case *))
            | _ => notObject jv
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
                  | get (_, []) = arrayBounds(i, jv)
                  | get (i, _::r) = get (i-1, r)
                in
                  if (i < 0) then arrayBounds(i, jv) else get (i, arr)
                end
            | _ => notArray jv
          (* end case *)))

    fun at path (D d) = D(fn jv => d (U.get(jv, path)))

    fun succeed x = D(fn _ => x)

    fun fail msg = D(fn jv => failure(msg, jv))

    fun andThen f (D d) = D(fn jv => decode (f (d jv)) jv)

    fun orElse (D d1, D d2) =
          (* try the first decoder.  If it fails with a `JSONError` exception, then
           * we try the second.
           *)
          D(fn jv => (d1 jv handle JSONError _ => d2 jv | ex => raise ex))

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
