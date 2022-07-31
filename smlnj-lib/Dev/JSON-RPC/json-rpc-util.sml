(* json-rpc-util.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Support for generating and parsing JSON RPC messages (Version 2.0).
 * See https://www.jsonrpc.org/specification for details on the specification.
 *
 * TODO: batch support
 *)

structure JSONRPCUtil : sig

  (* the JSON RPC version (= "2.0") *)
    val version : string

  (* Request IDs can either be numbers or strings *)
    datatype id = IdNum of int | IdStr of string

  (* `requestWithParams method (params, id)` returns a JSON RPC invocation-request message
   * for `method` with the given parameters (passed as a JSON array) and request ID.
   *)
    val requestWithParams : string -> JSON.value list * id -> JSON.value

  (* `requestWithNamedParams method (params, id)` returns a JSON RPC invocation-request
   * message for `method` with the given named parameters (passed as a JSON object) and
   * request ID.
   *)
    val requestWithNamedParams : string -> (string * JSON.value) list * id -> JSON.value

  (* `request method id` returns a JSON RPC invocation-request message
   * for `method` with the given request ID, but no parameters.
   *)
    val request : string -> id -> JSON.value

  (* `notifyWithParams method params` returns a JSON RPC notification request message
   *  with the given parameters (passed as a JSON array).
   *)
    val notifyWithParams : string -> JSON.value list -> JSON.value

  (* `notifyWithNamedParams method params` returns a JSON RPC notification request message
   * with the given named parameters (passed as a JSON object).
   *)
    val notifyWithNamedParams : string -> (string * JSON.value) list -> JSON.value

  (* `notify method` returns a JSON RPC notification request message that has
   * no parameters.
   *)
    val notify : string -> JSON.value

  (* `response (res, id)` returns a JSON RPC response message with the given
   * result and ID.
   *)
    val response  : JSON.value * id -> JSON.value

  (* `error (code, msg, optId)` returns an error message with the given code,
   * error message, and optional request ID.  If the ID is omitted, the `null`
   * is used for the ID.
   *)
    val error : int * string * id option -> JSON.value

  (* `errorWithData (code, msg, data, optId)` returns an error message with the given code,
   * message, extra data, and optional request ID.  If the ID is omitted, the `null`
   * is used for the ID.
   *)
    val errorWithData : int * string * JSON.value * id option -> JSON.value

  (* error codes that are specified by the JSON RPC spec *)
    val errInvalidJSON : int
    val errInvalidRequest : int
    val errMethodNotFound : int
    val errInvalidParams : int
    val errInternal : int

  (* the different kinds of request messages *)
    datatype request
      = Request of {
	    method : string,
	    params : JSON.value option,
	    id : id
	  }
      | Notify of {
	    method : string,
	    params : JSON.value option
	  }
      | InvalidReq

  (* `parseRequest v` parses the given JSON value as if it were a JSON request message
   * and returns the parsed information (or `InvalidResp` if it was not valid).
   *)
    val parseRequest : JSON.value -> request

  (* the different kinds of response messages *)
    datatype response
      = Response of {
	    result : JSON.value,
	    id : id
	  }
      | Error of {
	    code : int,
	    msg : string,
	    data : JSON.value option,
	    id : id option
	  }
      | InvalidResp

  (* `parseResponse v` parses the given JSON value as if it were a JSON response message
   * and returns the parsed information (or `InvalidResp` if it was not valid).
   *)
    val parseResponse : JSON.value -> response

  end = struct

    datatype value = datatype JSON.value

    val version = "2.0"

    datatype id = IdNum of int | IdStr of string

  (* all messages start with a version tag *)
    fun mkRPCObj flds = OBJECT(("jsonrpc", STRING version)::flds)

    fun mkId (IdNum id) = ("id", INT(IntInf.fromInt id))
      | mkId (IdStr s) = ("id", STRING s)

    fun requestWithParams method (params, id) = mkRPCObj [
	    ("method", STRING method),
	    ("params", ARRAY params),
	    mkId id
	  ]

    fun requestWithNamedParams method (params, id) = mkRPCObj [
	    ("method", STRING method),
	    ("params", OBJECT params),
	    mkId id
	  ]

    fun request method id = mkRPCObj [("method", STRING method), mkId id]

    fun notifyWithParams method params=  mkRPCObj [
	    ("method", STRING method),
	    ("params", ARRAY params)
	  ]

    fun notifyWithNamedParams method params = mkRPCObj [
	    ("method", STRING method),
	    ("params", OBJECT params)
	  ]

    fun notify method = mkRPCObj [("method", STRING method)]

    fun response (v, id) = mkRPCObj [("result", v), mkId id]

    fun error (code, msg, optId) = let
	  val err = ("error", OBJECT[
		  ("code", INT(IntInf.fromInt code)),
		  ("msg", STRING msg)
		])
	  val id = (case optId
		 of NONE => ("id", NULL)
		  | SOME id => mkId id
		(* end case *))
	  in
	    mkRPCObj [err, id]
	  end

    fun errorWithData (code, msg, data, optId) = let
	  val err = ("error", OBJECT[
		  ("code", INT(IntInf.fromInt code)),
		  ("msg", STRING msg),
		  ("data", data)
		])
	  val id = (case optId
		 of NONE => ("id", NULL)
		  | SOME id => mkId id
		(* end case *))
	  in
	    mkRPCObj [err, id]
	  end

  (* error codes that are specified by the JSON RPC spec *)
    val errInvalidJSON = ~32700
    val errInvalidRequest = ~32600
    val errMethodNotFound = ~32601
    val errInvalidParams = ~32602
    val errInternal = ~32603

  (* the different kinds of request messages *)
    datatype request
      = Request of {
	    method : string,
	    params : value option,
	    id : id
	  }
      | Notify of {
	    method : string,
	    params : value option
	  }
      | InvalidReq

  (* validate the version, which is assumed to be the first field and then parse
   * the fields (or invoke the error handler).
   *)
    fun validateVers (parse, _) (OBJECT(("jsonrpc", STRING "2.0")::fields)) = parse fields
      | validateVers (_, err) _ = err()

  (* processing request messages *)
    fun parseRequest v = let
	  fun err () = InvalidReq
	  fun parse1 (("method", STRING m)::flds) = parse2 (flds, m)
	    | parse1 _ = InvalidReq
	  and parse2 (("params", v as ARRAY _)::flds, m) = parse3 (flds, m, SOME v)
	    | parse2 (("params", v as OBJECT _)::flds, m) = parse3 (flds, m, SOME v)
	    | parse2 _ = InvalidReq
	  and parse3 ([], m, params) = Notify{method = m, params = params}
	    | parse3 ([("id", INT id)], m, params) = Request{
		  method = m, params = params, id = IdNum(IntInf.toInt id)
		}
	    | parse3 ([("id", STRING id)], m, params) = Request{
		  method = m, params = params, id = IdStr id
		}
	    | parse3 ([("id", NULL)], m, params) = Notify{method = m, params = params}
	    | parse3 _ = InvalidReq
	  in
	    validateVers (parse1, err) v
	  end

  (* the different kinds of response messages *)
    datatype response
      = Response of {
	    result : value,
	    id : id
	  }
      | Error of {
	    code : int,
	    msg : string,
	    data : value option,
	    id : id option
	  }
      | InvalidResp

  (* processing reply messages *)
    fun parseResponse v = let
	  fun err () = InvalidResp
	  fun parse1 (("result", v)::flds) = parseRes (flds, v)
	    | parse1 ([("error", OBJECT errFlds), ("id", INT id)]) =
		parseErr (errFlds, SOME(IdNum(IntInf.toInt id)))
	    | parse1 ([("error", OBJECT errFlds), ("id", STRING id)]) =
		parseErr (errFlds, SOME(IdStr id))
	    | parse1 ([("error", OBJECT errFlds), ("id", NULL)]) =
		parseErr (errFlds, NONE)
	    | parse1 _ = InvalidResp
	  and parseRes ([("id", INT id)], res) = Response{
		  result = res, id = IdNum(IntInf.toInt id)
		}
	    | parseRes ([("id", STRING id)], res) = Response{
		  result = res, id = IdStr id
		}
	    | parseRes _ = InvalidResp
	  and parseErr (("code", INT n)::flds, id) = parseErr1 (flds, IntInf.toInt n, id)
	    | parseErr _ = InvalidResp
	  and parseErr1 (("message", STRING msg)::flds, code, id) = parseErr2 (flds, code, msg, id)
	    | parseErr1 _ = InvalidResp
	  and parseErr2 ([], code, msg, id) = Error{
		  code = code, msg = msg, data = NONE, id = id
		}
	    | parseErr2 ([("data", v)], code, msg, id) = Error{
		  code = code, msg = msg, data = SOME v, id = id
		}
	    | parseErr2 _ = InvalidResp
	  in
	    validateVers (parse1, err) v
	  end

  end
