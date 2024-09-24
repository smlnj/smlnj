(* json-rpc-decode.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *
 * JSON RPC message decoding.
 *)

structure JSONRPCDecode : sig

    (* `request v` decodes the given JSON value as if it were a JSON request
     * message (or array of messages) and returns a list of requests.  There are
     * three possible results:
     *  1. an empty list, which signifies that the input was invalid
     *  2. a singleton request list
     *  3. a list of two or more requests (this corresponds to "batch mode")
     *)
    val request : JSON.value -> JSONRPC.request list

    (* `response v` decodes the given JSON value as if it were a JSON response
     * message (or array of messages) and returns the parsed information.  There
     * three possible results:
     *  1. an empty list, which signifies that the input was invalid
     *  2. a response request list
     *  3. a list of two or more response (this corresponds to "batch mode")
     *)
    val response : JSON.value -> JSONRPC.response list

  end = struct

    datatype value = datatype JSON.value
    datatype request = datatype JSONRPC.request
    datatype response = datatype JSONRPC.response
    datatype id = datatype JSONRPC.id

    (* helper function for decoding a single message or a batch of messages *)
    fun batch msgFn (ARRAY objs) = let
          fun lp ([], msgs) = List.rev msgs
            | lp (obj :: objs, msgs) = (case msgFn obj
                 of SOME msg => lp (objs, msg :: msgs)
                  | NONE => [] (* invalid message *)
                (* end case *))
          in
            lp (objs, [])
          end
      | batch msgFn obj = (case msgFn obj
           of SOME msg => [msg]
            | NONE => [] (* invalid message *)
          (* end case *))

    (* decode a single request message *)
    fun decodeRequest (OBJECT flds) = let
          fun parse (("jsonrpc", STRING v)::flds, vers, id, method, params) =
                setVersion (flds, v, vers, id, method, params)
            | parse (("id", INT n)::flds, vers, id, method, params) =
                setId (flds, IdNum(IntInf.toInt n), vers, id, method, params)
            | parse (("id", STRING s)::flds, vers, id, method, params) =
                setId (flds, IdStr s, vers, id, method, params)
            | parse (("method", STRING m)::flds, vers, id, method, params) =
                setMethod (flds, m, vers, id, method, params)
	    | parse (("params", p as ARRAY _)::flds, vers, id, method, params) =
                setParams (flds, p, vers, id, method, params)
	    | parse (("params", p as OBJECT _)::flds, vers, id, method, params) =
                setParams (flds, p, vers, id, method, params)
            | parse ([], true, SOME id, SOME method, params) =
                SOME(Request{method = method, params = params, id = id})
            | parse ([], true, NONE, SOME method, params) =
                SOME(Notify{method = method, params = params})
            | parse _ = NONE
          (* check and set the version field *)
          and setVersion (flds, v, vers, id, method, params) = (
                case (v, vers)
                 of ("2.0", false) => parse (flds, true, id, method, params)
                  | _ => NONE (* wrong version or multiple version fields *)
                (* end case *))
          (* set the ID field *)
          and setId (flds, id, vers, NONE, method, params) =
                parse (flds, vers, SOME id, method, params)
            | setId _ = NONE (* multiple id fields *)
          (* check the method field *)
          and setMethod (flds, m, vers, id, NONE, params) =
                parse (flds, vers, id, SOME m, params)
            | setMethod _ = NONE (* multiple method fields *)
          (* check the parameters field *)
          and setParams (flds, params, vers, id, method, NONE) =
                parse (flds, vers, id, method, SOME params)
            | setParams _ = NONE (* multiple params fields *)
	  in
	    parse (flds, false, NONE, NONE, NONE)
	  end
      | decodeRequest _ = NONE (* not an OBJECT *)

    (* decode one or more requests *)
    val request = batch decodeRequest

    (* decode a single response message *)
    fun decodeResponse (OBJECT flds) = let
          fun parse (("jsonrpc", STRING v)::flds, vers, id, result, error) =
                setVersion (flds, v, vers, id, result, error)
            | parse (("id", INT n)::flds, vers, id, result, error) =
                setId (flds, IdNum(IntInf.toInt n), vers, id, result, error)
            | parse (("id", STRING s)::flds, vers, id, result, error) =
                setId (flds, IdStr s, vers, id, result, error)
            | parse (("result", jv)::flds, vers, id, result, error) =
                setResult (flds, jv, vers, id, result, error)
	    | parse (("error", OBJECT errFlds)::flds, vers, id, result, error) =
                setError (flds, errFlds, vers, id, result, error)
            | parse ([], true, SOME id, SOME result, NONE) =
                SOME(Response{result = result, id = id})
            | parse ([], true, id, NONE, SOME(code, msg, data)) =
                SOME(Error{code = code, msg = msg, data = data, id = id})
            | parse _ = NONE
          (* check and set the version field *)
          and setVersion (flds, v, vers, id, response, error) = (
                case (v, vers)
                 of ("2.0", false) => parse (flds, true, id, response, error)
                  | _ => NONE (* wrong version or multiple version fields *)
                (* end case *))
          (* set the ID field *)
          and setId (flds, id, vers, NONE, response, error) =
                parse (flds, vers, SOME id, response, error)
            | setId _ = NONE (* multiple ID fields *)
          (* set the result field *)
          and setResult (flds, result, vers, id, NONE, error) =
                parse (flds, vers, id, SOME result, error)
            | setResult _ = NONE (* multiple ID fields *)
          (* parse and set the error field *)
          and setError (flds, errFlds, vers, id, result, NONE) = let
                fun parse' (("code", INT n)::eFlds, code, msg, data) =
                      setCode (eFlds, IntInf.toInt n, code, msg, data)
                  | parse' (("message", STRING s)::eFlds, code, msg, data) =
                      setMessage (eFlds, s, code, msg, data)
                  | parse' (("data", jv)::eFlds, code, msg, data) =
                      setData (eFlds, jv, code, msg, data)
                  | parse' ([], SOME code, SOME msg, data) =
                      parse (flds, vers, id, result, SOME(code, msg, data))
                  | parse' _ = NONE
                and setCode (eFlds, code, NONE, msg, data) =
                      parse' (eFlds, SOME code, msg, data)
                  | setCode _ = NONE
                and setMessage (eFlds, msg, code, NONE, data) =
                      parse' (eFlds, code, SOME msg, data)
                  | setMessage _ = NONE
                and setData (eFlds, data, code, msg, NONE) =
                      parse' (eFlds, code, msg, SOME data)
                  | setData _ = NONE
                in
                  parse' (errFlds, NONE, NONE, NONE)
                end
            | setError _ = NONE (* duplicate error fields *)
	  in
	    parse (flds, false, NONE, NONE, NONE)
	  end
      | decodeResponse _ = NONE (* not an OBJECT *)

    (* decode one or more responses *)
    val response = batch decodeResponse

  end
