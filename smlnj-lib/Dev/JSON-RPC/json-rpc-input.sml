(* json-rpc-input.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure JSONRPCInput : sig

    val parseRequest : substring -> JSONRPC.request list

    val parseResponse : substring -> JSONRPC.response list

  end = struct

    datatype value = datatype JSON.value
    datatype request = datatype JSONRPC.request
    datatype response = datatype JSONRPC.response
    datatype id = datatype JSONRPC.id

  end
