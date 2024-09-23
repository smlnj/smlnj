(* json-rpc-output.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure JSONRPCOutput : sig

    (* output a JSON RPC invocation-request message with an array of parameters *)
    val requestWithParams : {
            buf : CharBuffer.buf,       (* the output buffer *)
            method : string,            (* the method name for the request *)
            params : JSON.value list,   (* the parameters *)
            id : JSONRPC.id             (* the request ID *)
          } -> unit

    (* output a JSON RPC invocation-request message with named parameters *)
    val requestWithNamedParams : {
            buf : CharBuffer.buf,       (* the output buffer *)
            method : string,            (* the method name for the request *)
            params : (string * JSON.value) list,
                                        (* the named parameters *)
            id : JSONRPC.id             (* the request ID *)
          } -> unit

    (* output a JSON RPC invocation-request message w/o parameters *)
    val request : {
            buf : CharBuffer.buf,       (* the output buffer *)
            method : string,            (* the method name for the notification *)
            id : JSONRPC.id             (* the request ID *)
          } -> unit

    (* output a JSON RPC notification request message with an array of parameters *)
    val notifyWithParams : {
            buf : CharBuffer.buf,       (* the output buffer *)
            method : string,            (* the method name for the notification *)
            params : JSON.value list    (* the parameters *)
          } -> unit

    (* output a JSON RPC notification request message with named parameters *)
    val notifyWithNamedParams : {
            buf : CharBuffer.buf,       (* the output buffer *)
            method : string,            (* the method name for the notification *)
            params : (string * JSON.value) list
                                        (* the named parameters *)
          } -> unit

    (* output a JSON RPC notification request message w/o parameters *)
    val notify : {
            buf : CharBuffer.buf,       (* the output buffer *)
            method : string             (* the method name for the notification *)
          } -> unit

    (* output a JSON RPC response message with the given result and ID *)
    val response : {
            buf : CharBuffer.buf,       (* the output buffer *)
            result : JSON.value,        (* the result of the corresponding request *)
            id : JSONRPC.id             (* the ID of the corresponding request *)
          } -> unit

    (* `error (code, msg, optId)` returns an error message with the given code,
     * error message, and optional request ID.  If the ID is omitted, the `null`
     * is used for the ID, which is only used when the ID was not
     *)
    val error : {
            buf : CharBuffer.buf,       (* the output buffer *)
            code : int,                 (* the error code *)
            message : string,           (* the error message *)
            id : JSONRPC.id             (* the corresponding request ID *)
          } -> unit

    (* `errorWithData (code, msg, data, optId)` returns an error message with the given code,
     * message, extra data, and optional request ID.  If the ID is omitted, the `null`
     * is used for the ID.
     *)
    val errorWithData : {
            buf : CharBuffer.buf,       (* the output buffer *)
            code : int,                 (* the error code *)
            message : string,           (* the error message *)
            data : JSON.value,          (* the extra data *)
            id : JSONRPC.id             (* the corresponding request ID *)
          } -> unit

    (* `errorNoID {buf, code, message, data}` is used to output an error message
     * with no ID (`null` is used for the ID).  This function should only be used
     * when there was a problem parsing the request's ID.
     *)
    val errorNoID : {
            buf : CharBuffer.buf,       (* the output buffer *)
            code : int,                 (* the error code *)
            message : string,           (* the error message *)
            data : JSON.value option    (* the optional extra data *)
          } -> unit

  end = struct

    structure CB = CharBuffer
    structure F = Format

    datatype value = datatype JSON.value
    datatype id = datatype JSONRPC.id

    fun beginObj cb = CB.add1(cb, #"{")
    fun endObj cb = CB.add1(cb, #"}")
    fun beginArray cb = CB.add1(cb, #"[")
    fun endArray cb = CB.add1(cb, #"]")
    fun comma cb = CB.add1(cb, #",")

    (* the beginning of any JSON RPC message *)
    fun beginRPCMsg cb = CB.addVec(cb, "{\"jsonrpc\":\"2.0\",")

    (* output basic JSON values *)
    fun addNull cb = CB.addVec (cb, "null")
    fun addBool (cb, false) = CB.addVec (cb, "false")
      | addBool (cb, true) = CB.addVec (cb, "true")
    fun addInt (cb, n) = CB.addVec (cb, F.format "%d" [F.INT n])
    fun addInteger (cb, n) = CB.addVec (cb, F.format "%d" [F.LINT n])
    fun addFloat (cb, f) = let
          (* print with 17 digits of precision, which is sufficient for any
           * double-precision IEEE float.  We first convert to a string using
           * SML syntax and then replace any "~" characters with "-".
           *)
          val s = Real.fmt (StringCvt.GEN(SOME 17)) f
          in
            if CharVector.exists (fn #"~" => true | _ => false) s
              then CB.addVec (cb, String.map (fn  #"~" => #"-" | c => c) s)
              else CB.addVec (cb, s)
          end
    fun addString (cb, s) = let
	  fun getChar i = if (i < size s) then SOME(String.sub(s, i), i+1) else NONE
	  val getWChar = UTF8.getu getChar
	  fun tr (i, chrs) = (case getWChar i
		 of SOME(wchr, i) => if (wchr <= 0w126)
		      then let
			val c = (case UTF8.toAscii wchr
			       of #"\"" => "\\\""
				| #"\\" => "\\\\"
				| #"\b" => "\\b"
				| #"\f" => "\\f"
				| #"\n" => "\\n"
				| #"\r" => "\\r"
				| #"\t" => "\\t"
				| c => if (wchr < 0w32)
				    then F.format "\\u%04x" [F.WORD wchr]
				    else str c
			      (* end case *))
			in
			  tr (i, c :: chrs)
			end
		      else tr(i, F.format "\\u%04x" [F.WORD wchr] :: chrs)
		  | NONE => String.concat(List.rev chrs)
		(* end case *))
	  in
	    CB.addVec (cb, F.format "\"%s\"" [F.STR(tr (0, []))])
	  end

    (* add a comma-separated list of elements to the buffer *)
    fun addList (addElem, cb, xs) = let
          fun add [] = ()
            | add [x] = addElem (cb, x)
            | add (x::r) = (
                addElem (cb, x);
                comma cb;
                add r)
          in
            add xs
          end

    (* add a JSON value to the buffer *)
    fun addValue (cb, OBJECT fields) = (
          beginObj cb;
          addList (addNamedValue, cb, fields);
          endObj cb)
      | addValue (cb, ARRAY vs) = (
          beginArray cb;
          addList (addValue, cb, vs);
          endArray cb)
      | addValue (cb, NULL) = addNull cb
      | addValue (cb, BOOL b) = addBool(cb, b)
      | addValue (cb, INT n) = addInteger (cb, n)
      | addValue (cb, FLOAT f) = addFloat (cb, f)
      | addValue (cb, STRING s) = addString (cb, s)

    (* add a labeled JSON value to the buffer *)
    and addNamedValue (cb, (label, value)) = (
          addString (cb, label);
          CB.add1 (cb, #":");
          addValue (cb, value))

    fun addID (cb, IdNum id) = (CB.addVec(cb, "\"id\":"); addInt(cb, id))
      | addID (cb, IdStr s) = (CB.addVec(cb, "\"id\":"); addString(cb, s))

    fun addOptionalID (cb, NONE) = CB.addVec(cb, "\"id\":null")
      | addOptionalID (cb, SOME id) = addID(cb, id)

    fun addMethod (cb, method) = (CB.addVec(cb, "\"method\":"); addString(cb, method))

    fun requestWithParams {buf, method, params, id} = (
          beginRPCMsg buf;
            addID (buf, id); comma buf;
            addMethod (buf, method); comma buf;
            CB.addVec(buf, "\"params\":[");
              addList (addValue, buf, params);
            endArray buf;
          endObj buf)

    fun requestWithNamedParams{buf, method, params, id} = (
          beginRPCMsg buf;
            addID (buf, id); comma buf;
            addMethod (buf, method); comma buf;
            CB.addVec(buf, "\"params\":{");
              addList (addNamedValue, buf, params);
            endObj buf;
          endObj buf)

    fun request {buf, method, id} = (
          beginRPCMsg buf;
            addID (buf, id); comma buf;
            addMethod (buf, method);
          endObj buf)

    fun notifyWithParams {buf, method, params} = (
          beginRPCMsg buf;
            addMethod (buf, method); comma buf;
            CB.addVec(buf, "\"params\":[");
              addList (addValue, buf, params);
            endArray buf;
          endObj buf)

    fun notifyWithNamedParams {buf, method, params} = (
          beginRPCMsg buf;
            addMethod (buf, method); comma buf;
            CB.addVec(buf, "\"params\":{");
              addList (addNamedValue, buf, params);
            endObj buf;
          endObj buf)

    fun notify {buf, method} = (
          beginRPCMsg buf;
            addMethod (buf, method);
          endObj buf)

    fun response {buf, result, id} = (
          beginRPCMsg buf;
            addID (buf, id); comma buf;
            CB.addVec(buf, "\"result\":"); addValue(buf, result);
          endObj buf)

    fun error {buf, code, message, id} = (
          beginRPCMsg buf;
            addID (buf, id); comma buf;
            CB.addVec(buf, "\"error\":{");
              CB.addVec(buf, "\"code\":"); addInt(buf, code); comma buf;
              CB.addVec(buf, "\"message\":"); addString(buf, message);
            endObj buf;
          endObj buf)

    fun errorWithData {buf, code, message, data, id} = (
          beginRPCMsg buf;
            addID (buf, id); comma buf;
            CB.addVec(buf, "\"error\":{");
              CB.addVec(buf, "\"code\":"); addInt(buf, code); comma buf;
              CB.addVec(buf, "\"message\":"); addString(buf, message); comma buf;
              CB.addVec(buf, "\"data\":"); addValue(buf, data);
            endObj buf;
          endObj buf)

    fun errorNoID {buf, code, message, data} = (
          beginRPCMsg buf;
            CB.addVec(buf, "\"id\":null,"); (* null ID *)
            CB.addVec(buf, "\"error\":{");
              CB.addVec(buf, "\"code\":"); addInt(buf, code); comma buf;
              CB.addVec(buf, "\"message\":"); addString(buf, message);
              case data (* data is optional *)
               of SOME data => (CB.addVec(buf, ",\"data\":"); addValue(buf, data))
                | NONE => ()
              (* end case *);
            endObj buf;
          endObj buf)

  end
