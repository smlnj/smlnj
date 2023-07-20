(* cm/smlfile/dbm/ietree.sml *)

(* import-export trees, a potential replaceent for skeletons *)

(* "provisional" imports, or pimports:
 * one OpenScope for each level of nested open declarations *)
type pimports =
  = OpenScope of sympath list * symset * pimports
     (* corresponding to the "scope" of an open dec, with nesting;
      * -- the sympath list are the sympaths "opened" at the most local
      * open dec;
      * -- the symset is the set of str symbols appearing free
      *    "here", in this "scoping layer", i.e. in the interval between
      *    _this_ open and the next inner open. (???) *)
  | NOimports

(* We "push" an Openscope as we process an open dec. with the set of
   provisional import symbols (free path heads) detected in its "exclusive
   scope".
*)


(* alternatively: *)
type pimport =
  = Open of sympath list
  | Simple of symset

type pimport =
  = Open of sympath list * import set
  | Simple of import set

type pimports = pimport list	


(* node of an ietree, representing a declared structure or substructure *)
datatype node
  = N of
   {name: symbol,  (* name of str/sig, locally bound or free *)
    comps: component list,  (* substructures *)
    imports: imports}
   (* complete: bool -- all components known (no opens to be resolved) *)

and component
  = Sub of node      -- declared substructure
  | Open of sympath  -- opened structure
       (* an open dec introduces possibly multiple, possibly unknown (free)
	* symbol bindings *)

(* NOTES

@ Two parallel constructs with related issues:
   * open declarations (in structures, possibly localized)
   * include specs in signatures
 These both introduce implicit symbol bindings that need to eventually
 be resolved to determine which symbol occurrences are actually free
 (implying imported).

 The opened path may be "local" or "free", depending on whether its path head
 has been locally declared (previously in the c-unit).
 
*)
