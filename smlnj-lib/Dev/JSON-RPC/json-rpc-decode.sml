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

    (* validate the version, which is assumed to be the first field and then parse
     * the fields (or invoke the error handler).
     *)
    fun validateVers parse (OBJECT(("jsonrpc", STRING "2.0")::fields)) = parse fields
      | validateVers _ _ = NONE

    (* decode a single request message *)
    fun decodeRequest v = let
	  fun err () = NONE
	  fun parse1 (("method", STRING m)::flds) = parse2 (flds, m)
	    | parse1 _ = NONE
	  and parse2 (("params", v as ARRAY _)::flds, m) = parse3 (flds, m, SOME v)
	    | parse2 (("params", v as OBJECT _)::flds, m) = parse3 (flds, m, SOME v)
	    | parse2 _ = NONE
	  and parse3 ([], m, params) = SOME(Notify{method = m, params = params})
	    | parse3 ([("id", INT id)], m, params) = SOME(Request{
		  method = m, params = params, id = IdNum(IntInf.toInt id)
		})
	    | parse3 ([("id", STRING id)], m, params) = SOME(Request{
		  method = m, params = params, id = IdStr id
		})
	    | parse3 ([("id", NULL)], m, params) =
                SOME(Notify{method = m, params = params})
	    | parse3 _ = NONE
	  in
	    validateVers parse1 v
	  end

    (* decode one or more requests *)
    val request = batch decodeRequest

    (* decode a single response message *)
    fun decodeResponse v = let
	  fun err () = NONE
	  fun parse1 (("result", v)::flds) = parseRes (flds, v)
	    | parse1 ([("error", OBJECT errFlds), ("id", INT id)]) =
		parseErr (errFlds, SOME(IdNum(IntInf.toInt id)))
	    | parse1 ([("error", OBJECT errFlds), ("id", STRING id)]) =
		parseErr (errFlds, SOME(IdStr id))
	    | parse1 ([("error", OBJECT errFlds), ("id", NULL)]) =
		parseErr (errFlds, NONE)
	    | parse1 _ = NONE
	  and parseRes ([("id", INT id)], res) = SOME(Response{
		  result = res, id = IdNum(IntInf.toInt id)
		})
	    | parseRes ([("id", STRING id)], res) = SOME(Response{
		  result = res, id = IdStr id
		})
	    | parseRes _ = NONE
	  and parseErr (("code", INT n)::flds, id) =
                parseErr1 (flds, IntInf.toInt n, id)
	    | parseErr _ = NONE
	  and parseErr1 (("message", STRING msg)::flds, code, id) =
                parseErr2 (flds, code, msg, id)
	    | parseErr1 _ = NONE
	  and parseErr2 ([], code, msg, id) = SOME(Error{
		  code = code, msg = msg, data = NONE, id = id
		})
	    | parseErr2 ([("data", v)], code, msg, id) = SOME(Error{
		  code = code, msg = msg, data = SOME v, id = id
		})
	    | parseErr2 _ = NONE
	  in
	    validateVers parse1 v
	  end

    (* decode one or more responses *)
    val response = batch decodeResponse

  end
