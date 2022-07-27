(* abs.sml
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure MLABS =
struct
  type atom = Atom.atom
  datatype raw_ml = Raw of ml_token list

  and ml_token = Tok of string
			
  datatype cmp_op = LT | GT | EQ | LEQ | GEQ
  datatype bool_op = AND | OR

  datatype ml_file 
    = OBJECT of ml_object

  and ml_object
    = DECL of ml_decl list
    | STRUCTURE of ml_signature option * ml_structure
    | LOCAL of ml_object * ml_object

  and ml_decl
    = D_VAL of (ml_pat * ml_exp) list
    | D_FUN of ml_fundecl list
    | D_TYPE of ml_type_binding
    | D_ABS_DATATYPE of ml_datatype_binding * ml_type_binding option 
          (* datatype $1 withtype $2*)
    | D_ABS_TYPE of ml_datatype_binding * ml_type_binding option * ml_decl list
             (* abstype $1 withtype $2 with $3 end *)
(*  | SD_LOCAL of ()
    | SD_OPEN of ()
    | SD_OPERATOR of ()
*)

  and ml_fundecl = FUN of (ml_fun_heading * ml_type option * ml_exp) list (* none-empty list *)

  and ml_type_binding 
    = TypeBind of (typevar list * ml_type) list (* none-empty *)

  and ml_datatype_binding 
    = DatatypeBind of (typevar list * (id * ml_type option) list (* none-empty*)) list (* none-empty *)

  and ml_fun_heading 
    = FUNHEAD of atom * ml_pat list (* none-empty *)

  and ml_type
    (*ml_type::= Type_Var | Type_Var_List Compound_Ident | Type "*" L(Type , "*") | Type "->" Type | "{" O_List( Label ":" Type ) "}" | "(" Type ")". *)
    = T_VAR of typevar
    | T_TUPLE of ml_type list (* NoneEmptyList *) (*   t1 * t2 * .. *)
    | T_FUN of ml_type * ml_type (* t1-> t2 *)
    | T_RECORD of (label * ml_type) list
    | T_PAREN of ml_type

  and ml_signature (*signature*)
    (* Signature::= "sig" Specification "end" | Ident.*)
    = SG of ml_specification (* fix this *)

  and ml_specification 
    = SP_EMPTY (* fix this *)
  (*Specification::= Empty | value_spec | various_type_spec | exception_spec | structure_spec | other_spec | inclusion | Specification O(";") Specification. *)

  and ml_structure (*structure*)
    = ST_STRUCT of ml_object (* struct $1 end *)
    (* Structure::= "struct" Object_Declaration "end" | Compound_Ident | Functor_Application | Ident "(" Object_Declaration ")" | "let" Object_Declaration "in" Structure "end".*)

  (* a subset of ML expressions and patterns *)
  and ml_exp
    = ML_Var of string
    | ML_Int of IntInf.int
    | ML_Cmp of (cmp_op * ml_exp * ml_exp)
    | ML_Bool of (bool_op * ml_exp * ml_exp)
    | ML_Case of ml_exp * (ml_pat * ml_exp) list
    | ML_If of ml_exp * ml_exp * ml_exp
    | ML_App of (string * ml_exp list)
    | ML_Let of (string * ml_exp * ml_exp)
    (* a group of mutually-recursive functions *)
    | ML_Funs of (string * string list * ml_exp) list * ml_exp
    | ML_Seq of ml_exp list
    | ML_Tuple of ml_exp list
    | ML_List of ml_exp list
    | ML_RefGet of ml_exp
    | ML_RefPut of ml_exp * ml_exp
    | ML_Handle of ml_exp * (ml_pat * ml_exp) list
    | ML_Raw of ml_token list
  (* the following added by Chunyan *)
    | ML_TypeExp of ml_exp * ml_type
		
  and ml_pat
    = ML_Wild
    | ML_VarPat of string
    | ML_IntPat of IntInf.int
    | ML_ConPat of string * ml_pat list
    | ML_TupPat of ml_pat list
    (* the following added by Chunyan *)
    | ML_TypePat of ml_pat * ml_type 
    | ML_ListPat of ml_pat list
		    
(*
  and pattern 
    (* Pattern::= Atomic_Pattern | Compound_Name Atomic_Pattern | Pattern infix_constructor Pattern | Pattern ":" ml_type | Name O(":" ml_type) "as" Pattern. *)
    = P_ATOM of atomic_pattern
    | P_TYPE of pattern * ml_type
    (* more *)
  and atomic_pattern
    = AP_NULL  (* "_" *)
    | AP_COMPNAME of id
    (*    | P_CONST  why?? *)
    | AP_TUPLE of pattern list
    | AP_LIST of pattern list
    | AP_RECORD of (label * pattern) list 
  (* more? *)

  and exp
    (* # Expression::= Infix_Expression | Expression ":" ml_type | Expression boolean_operator Expression | Expression "handle" Match | "raise" Expression | selection| loop | "fn" Match. *)
    (* atomic expressions *)
    = E_COMPNAME of atom 
    | E_CONST of const
    | E_TUPLE of exp list
    | E_LIST of exp list (* NoneEmptyList *)
    | E_RECORD of (label * exp) list
    | E_STMT of exp list (* NoneEmptyList *) (* e1; e2;... *)
    | E_LET of decl * exp list (* NoneEmptyList *)
    (* *)
    | E_TYPE of exp * ml_type
    | E_APPLY of exp list (* NoneEmptyList *)
    | E_BOOLEAN of exp * boolop * exp 
    | E_HANDLE of exp * match 
    | E_RAISE of exp 
(*
    | E_SELECTION of selection
    | E_LOOP of loop 
*)
    | E_FN of match 
  and const
    = C_INT of int
    | C_REAL of real
    | C_STRING of string 
  (* ... *)

  and boolop
    = B_AND
    | B_OR 
*)
  withtype label = atom 
  and id = atom 
  and typevar = atom (* fix this *)
  and match = (ml_pat * ml_exp) list (* NoneEmptyList *)

end