(* json-rpc-encode.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *
 * JSON RPC message encoding.
 *)

structure JSONRPCEncode : sig

    (* `requestWithParams {method, params, id}` returns a JSON object that represents
     * a JSON-RPC invocation-request message for `method` with the given parameters
     * (passed as a JSON array) and request ID.
     *)
    val requestWithParams : {
            method : string,            (* the method name for the request *)
            params : JSON.value list,   (* the parameters *)
            id : JSONRPC.id             (* the request ID *)
          } -> JSON.value

    (* `requestWithNamedParams {method, params, id}` returns a JSON object that
     * represents a JSON-RPC invocation-request message for `method` with the given
     * named parameters (passed as a JSON object) and request ID.
     *)
    val requestWithNamedParams : {
            method : string,            (* the method name for the request *)
            params : (string * JSON.value) list,
                                        (* the named parameters *)
            id : JSONRPC.id             (* the request ID *)
          } -> JSON.value

    (* `request {method, id}` returns a JSON object that represents a JSON-RPC
     * invocation-request message for `method` with the given request ID, but
     * no parameters.
     *)
    val request : {
            method : string,            (* the method name for the notification *)
            id : JSONRPC.id             (* the request ID *)
          } -> JSON.value

    (* `notifyWithParams {method, params}` returns a JSON object that represents
     * a JSON-RPC notification request message with the given parameters (passed
     * as a JSON array).
     *)
    val notifyWithParams : {
            method : string,            (* the method name for the notification *)
            params : JSON.value list    (* the parameters *)
          } -> JSON.value

    (* `notifyWithNamedParams method params` returns a JSON object that represents
     * a JSON-RPC notification request message with the given named parameters
     * (passed as a JSON object).
     *)
    val notifyWithNamedParams : {
            method : string,            (* the method name for the notification *)
            params : (string * JSON.value) list
                                        (* the named parameters *)
          } -> JSON.value

    (* `notify method` returns a JSON object that represents a JSON-RPC notification
     * request message that has no parameters.
     *)
    val notify : {
            method : string             (* the method name for the notification *)
          } -> JSON.value

    (* `response (res, id)` returns a JSON object that represents a JSON-RPC
     * response message with the given result and ID.
     *)
    val response :{
            result : JSON.value,        (* the result of the corresponding request *)
            id : JSONRPC.id             (* the ID of the corresponding request *)
          } -> JSON.value

    (* `error (code, msg, optId)` returns a JSON object that represents a
     * JSON-RPC error message with the given code, error message, and request ID.
     *)
    val error : {
            code : int,                 (* the error code *)
            message : string,           (* the error message *)
            id : JSONRPC.id             (* the corresponding request ID *)
          } -> JSON.value

    (* `errorWithData (code, msg, data, optId)` returns a JSON object that
     * represents a JSON-RPC error message with the given code, message, extra
     * data, and request ID.
     *)
    val errorWithData : {
            code : int,                 (* the error code *)
            message : string,           (* the error message *)
            data : JSON.value,          (* the extra data *)
            id : JSONRPC.id             (* the corresponding request ID *)
          } -> JSON.value

    (* `errorNoID {buf, code, message, data}` ireturns a JSON object that
     * represents a JSON-RPC error message with no ID (`null` is used for the ID).
     * This function should only be used when there was a problem parsing the
     * corresponding request's ID.
     *)
    val errorNoID : {
            code : int,                 (* the error code *)
            message : string,           (* the error message *)
            data : JSON.value option    (* the optional extra data *)
          } -> JSON.value

  end = struct

    datatype value = datatype JSON.value
    datatype request = datatype JSONRPC.request
    datatype response = datatype JSONRPC.response
    datatype id = datatype JSONRPC.id

    (* all messages start with a version tag *)
    fun mkRPCObj flds = OBJECT(("jsonrpc", STRING JSONRPC.versionString)::flds)

    fun mkId (IdNum id) = ("id", INT(IntInf.fromInt id))
      | mkId (IdStr s) = ("id", STRING s)

    fun requestWithParams {method, params, id} = mkRPCObj [
	    mkId id,
	    ("method", STRING method),
	    ("params", ARRAY params)
	  ]

    fun requestWithNamedParams {method, params, id} = mkRPCObj [
	    mkId id,
	    ("method", STRING method),
	    ("params", OBJECT params)
	  ]

    fun request {method, id} = mkRPCObj [mkId id, ("method", STRING method)]

    fun notifyWithParams {method, params} = mkRPCObj [
	    ("method", STRING method),
	    ("params", ARRAY params)
	  ]

    fun notifyWithNamedParams {method, params} = mkRPCObj [
	    ("method", STRING method),
	    ("params", OBJECT params)
	  ]

    fun notify {method} = mkRPCObj [("method", STRING method)]

    fun response {result, id} = mkRPCObj [mkId id, ("result", result)]

    fun error {code, message, id} = mkRPCObj [
            mkId id,
            ("error", OBJECT[
                ("code", INT(IntInf.fromInt code)),
                ("message", STRING message)
              ])
          ]

    fun errorWithData {code, message, data, id} = mkRPCObj [
            mkId id,
            ("error", OBJECT[
                ("code", INT(IntInf.fromInt code)),
                ("message", STRING message),
                ("data", data)
              ])
          ]

    fun errorNoID {code, message, data} = mkRPCObj [
            ("id", NULL),
	    ("error", OBJECT(
                ("code", INT(IntInf.fromInt code)) ::
                ("message", STRING message) ::
                (case data of SOME data => [("data", data)] | NONE => [])
              ))
          ]

  end
