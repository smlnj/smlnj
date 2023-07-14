(* cm/smlfile/dbm/ietree.sml *)

(* import-export trees *)

(* imports: one OpenScope for each level of nested open declarations *)
type imports =
  = OpenScope of sympath list * symset * imports
  | NOimports

type import =
  = Open of sympath list
  | Simple of symset

type import =
  = Open of sympath list * import set
  | Simple of import set

type imports = import list	

(* node of an ietree *)
datatype node
  = N of {name: symbol, comps: component list, imports: imports}
   (* complete: bool -- all components known (no opens to be resolved) *)

and component
  = Sub of node      -- declared substructure
  | Open of sympath  -- opened structure

