(* FLINT/match/paths.sml *)

structure Paths =
struct

local
  structure T = Types
  structure TU = TypesUtil
  structure AS = Absyn
  structure AU = AbsynUtil
in

(* --------------------------------------------------------------------------- *)
(* con: case discriminators *)

datatype link
  = PI of int  (* indexed record/tuple selection *)
  | VI of int * Types.ty (* indexed vector selection, with vector element type *)
  | DC of AS.con  (* datacon/constant/vector-length discriminator *)

type path = link list   (* from root down to node *)
type rpath = link list  (* from node back to root *)

val rootpath: path = nil
val rootrpath: rpath = nil

(* utility functions for printing (conversion to strings) *)

(* linkToString : link -> string *)
fun linkToString (PI n) = "PI:" ^ Int.toString n
  | linkToString (VI (n,_)) = "VI:" ^ Int.toString n
  | linkToString (DC (con as AS.VLENcon _)) = "VL[" ^ AU.conToString con ^ "]"
  | linkToString (DC con) = "DC[" ^ AU.conToString con ^ "]"

(* pathToString : path -> string *)
fun pathToString (path: path) =
    PrintUtil.listToString ("[", ",", "]") linkToString path

(* pathToList : path -> link list *)
fun pathToList (p: path) = p

(* pathToRpath : path -> rpath *)
fun pathToRpath (p: path) : rpath = rev p

(* rpathToPath : rpath -> path *)
fun rpathToPath (rp: rpath) : path = rev rp

(* pathLength : path -> int *)
fun pathLength (p: path) = length p

(* addLink : link * path -> path *)
fun addLink (link: link, path: path) = 
(*    let val result = path @ [link]
    in print "addLink: "; print (pathToString result); print "\n";
       result
    end *)
   path @ [link]

(* addLinkR : link * rpath -> rpath *)
fun addLinkR (link: link, rpath: rpath) = 
(*    let val result = link :: rpath
    in print "addLinkR: "; print (pathToString (rpathToPath result)); print "\n";
       result
    end *)
    link::rpath

fun eqLink (PI n1, PI n2) = (n1 = n2)
  | eqLink (VI (n1,_), VI (n2,_)) = (n1 = n2)  (* types assumed to agree *)
  | eqLink (DC con1, DC con2) = AU.eqCon (con1, con2)
  | eqLink _ = false

fun eqPath (nil, nil) = true
  | eqPath (link1::rest1, link2::rest2) = 
      eqLink (link1, link2) andalso eqPath (rest1, rest2)
  | eqPath _ = false

(* prefix : path * path -> bool *)
fun prefix (nil, _) = true
  | prefix (_, nil) = false
  | prefix (link1::rest1, link2::rest2) = 
    eqLink (link1, link2) andalso prefix (rest1, rest2)

(* suffix : path * path -> link list option *)
(*  returns SOME links if path1 @ links = path2, implying path1 is a prefix of path2 *)
fun suffix (path1: path, path2: path) =
    let fun strip (nil, path) = SOME (pathToList path)
	  | strip (_, nil) = NONE
	  | strip (link1::rest1, link2::rest2) = 
	      if eqLink (link1, link2)
	      then strip (rest1, rest2)
	      else NONE
    in strip (path1, path2)
    end				 


end (* top local *)
end (* structure Paths *)
