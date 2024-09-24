(* json-rpc-input.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure JSONRPCInput : sig

    val request : string -> JSONRPC.request list

    val response : string -> JSONRPC.response list

  end = struct

    datatype value = datatype JSON.value
    datatype request = datatype JSONRPC.request
    datatype response = datatype JSONRPC.response
    datatype id = datatype JSONRPC.id

    fun request content =
          JSONRPCDecode.request(JSONParser.parse(JSONParser.openString content))

    fun response content =
          JSONRPCDecode.response(JSONParser.parse(JSONParser.openString content))

  end
