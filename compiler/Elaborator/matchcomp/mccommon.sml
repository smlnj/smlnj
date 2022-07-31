(* mccommon.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* This contains the types used by the match compiler.
 * TODO: this module requires a signature! (which would reiterate the structure) *)

structure MCCommon =
struct

local
  structure DA = Access
  structure T = Types
  structure AS = Absyn
  structure P = Paths
in

(* --------------------------------------------------------------------------- *)
(* ruleno and ruleset types *)

structure RuleSet = IntListSet
structure RS = RuleSet (* abbreviation *)

type ruleno = RuleSet.item (* = int *)
type ruleset = RuleSet.set

(* --------------------------------------------------------------------------- *)
(* Andor trees *)

(* datatype subcase:
 *  In an OR (or ORp) case node split, the discrimination is on a con discriminator
 *  and the "payload" of a particular case or "variant" is expressed as a "subcase",
 *  which has three forms according to whether the dicriminator con is a constant (CONST)
 *  a non-constant datatype constructor taking an argument (DCARG), or a vector length
 *  discriminator (VLENcon), taking a list of vector elements (VELEMS).
*)
datatype 'a subcase  (* kinds of andor0/andor subcases *)
  = CONST            (* discriminant is int, word, string, or constant datacon *)
  | DCARG of 'a       (* discriminant is non-constant datacon, 'a is its argument *)
  | VELEMS of 'a list   (* discrimanant is vector length, 'a list are the vector elements *)

(* datatype protoAndor
 *   "proto" or "basic" AndOr trees, built by the first pass 1*)
datatype protoAndor
  = ANDp of
     {varRules : ruleset,           (* rules having a variable at this node *)
      children : protoAndor list}
  | ORp of
     {varRules : ruleset,
      sign : DA.consig,
      cases : protoVariant list}
  | VARp of
     {varRules : ruleset}
  | WCp
withtype protoVariant = AS.con * ruleset * protoAndor subcase

(* nodeId:
 *  node id numbers are added in the 2nd pass of andor tree construction (performed
 *  in translateAndor). These are used as a more efficient node identifier than paths,
 *  and useful for forming efficient maps or environments over nodes (e.g. mvarenv). *)
type nodeId = int

datatype andor
  = AND of  (* implicit record/tuple type, #fields = length children *)
      {id : nodeId,
       children: andor list} (* tuple/record components *)
  | OR of  (* case descrimination or 1-level switch *)
      {id: nodeId,
       path: P.path,         (* path to this node *)
       sign : DA.consig,     (* retained in dectree:SWITCH, eventually passed to SWITCH;
			      *  could replace with type *)
       defaults: ruleset,    (* "default" rules that match because of VAR or WC *)
       cases: variant list}  (* case "variants" *)
  | VAR of  (* variable node, not overlayed onto and ANO or OR node -- a leaf node *)
      {id : nodeId}
  | WC  (* WILDCARD: like VAR, but no nodeId? *)
withtype variant = AS.con * ruleset * andor subcase


(* --------------------------------------------------------------------------- *)
(* decision tree *)

(* See Sec 40.9, 41.2 in match-compiler notes *)
datatype dectree
  = SWITCH of
      {id : nodeId,     (* the id of the andor OR node that is the basis of the SWITCH *)
       path : P.path,   (* the path of the underlying andor OR node *)
       sign: DA.consig, (* used to determine datatype width, but also passed to PL.SWITCH
                         * in Generate..genswitch. Could be replaced by the type itself. *)
       cases: (AS.con * dectree) list, (* SWITCH is _saturated_ if cons are exhaustive for type *)
       defaultOp: dectree option,     (* possible default, NONE if cases are saturated *)
       live: ruleset}   (* rules live at this node, used to qualify relevant rules for mvar binding *)
  | RHS of ruleno  (* match succeeds: martial variable values and dispatch to rhs(ruleno) *)
  | FAIL           (* match fails, raise Match/Bind exn, or reraise unmatched exn in handler *)

end (* toplevel local *)
end (* structure MCCommon *)
