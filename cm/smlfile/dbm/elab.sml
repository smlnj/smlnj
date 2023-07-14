(* cm/smlfile/dbm/elab.sml *)

(* elaborating structure dec Ast into ietree *)

type strenv = (str)symbol -> ietree

type env = strenv * sympath set  (* opened paths *)

(* val elab : Ast.strdec * env -> ietree * imports *)
