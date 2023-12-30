(* json-decode-sig.sml
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature JSON_DECODE =
  sig

    datatype 'a result = Err of exn | Ok of 'a

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

    type 'a decoder

    val decode : 'a decoder -> JSON.value -> 'a result
    val decodeString : 'a decoder -> string -> 'a result

    val bool : bool decoder
    val int : int decoder
    val intInf : IntInf.int decoder
    val number : Real64.real decoder
    val string : string decoder
    val null : 'a -> 'a decoder

    (* returns the raw JSON value without further decoding *)
    val raw : JSON.value decoder

    (* returns a decoder that maps the JSON `null` value to `Ok NONE` and otherwise
     * decodes the value using the supplied decoder.
     *)
    val nullable : 'a decoder -> 'a option decoder

    (* decodes a JSON ARRAY into a list of values *)
    val list : 'a decoder -> 'a list decoder

    (* returns a decoder that attempts to decode a value and returns `Ok NONE`
     * on failure (instead of an error result).
     *)
    val try : 'a decoder -> 'a option decoder

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

    (* `sub i d` returns a decoder that when given a JSON array, decodes the i'th
     * array element.
     *)
    val sub : int -> 'a decoder -> 'a decoder

    (* returns a decoder that decodes the value at the location specified by
     * the path.
     *)
    val at : JSONUtil.path -> 'a decoder -> 'a decoder

    (* `succeed v` returns a decoder that always yields `Ok v` for any JSON input *)
    val succeed : 'a -> 'a decoder

    (* `fail msg` returns a decoder that returns `Err(Failure(msg, jv))` for
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
    val map2 : ('a * 'b -> 'c)
          -> ('a decoder * 'b decoder)
          -> 'c decoder
    val map3 : ('a * 'b * 'c -> 'd)
          -> ('a decoder * 'b decoder * 'c decoder)
          -> 'd decoder

  end
