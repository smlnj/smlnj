(* json-rpc.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure JSONRPC =
  struct

    (* We support version 2.0 *)
    val version = [2, 0]
    val versionString = String.concatWithMap "." Int.toString version

    (* Request IDs can either be numbers or strings *)
    datatype id = IdNum of int | IdStr of string

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

    (* error codes that are specified by the JSON RPC spec *)
    val errInvalidJSON : int = ~32700
    val errInvalidRequest : int = ~32600
    val errMethodNotFound : int = ~32601
    val errInvalidParams : int = ~32602
    val errInternal : int = ~32603

  end
