(* Copyright 1989 by AT&T Bell Laboratories *)
(* tuples.sml *)

(*
 * TUPLES and Tuples should be called RECORDS and Records, since
 * records are the primary concept, and tuples are a derived form.
 *)
signature TUPLES =
sig

  val mkTUPLEtyc : int -> Types.tycon
  val isTUPLEtyc : Types.tycon -> bool
  val mkRECORDtyc : Types.label list -> Types.tycon
  val mkTUPLEtype : Types.ty list -> Types.ty

end  (* signature TUPLES *)

structure Tuples : TUPLES =
struct

local (* top local *)

  structure S = Symbol
  structure NL = NumericLabel
  structure T = Types

  structure TyconArray =
    DynamicArrayFn
      (struct
	 open Array
	 type array = T.tycon option array
	 type vector = T.tycon option vector
	 type elem = T.tycon option
	end)

  val tupleTycons = TyconArray.array (0, NONE)

  structure Tbl = WordStringHashTable

  open Types

in

exception New
val tyconTable = Tbl.mkTable (32, New) : tycon Tbl.hash_table
val tyconMap = Tbl.lookup tyconTable
val tyconAdd = Tbl.insert tyconTable

(* labelsToSymbol : label list -> S.symbol *)
fun labelsToSymbol(labels: label list) : Symbol.symbol =
    let fun wrap [] = ["}"]
	  | wrap [id] = [Symbol.name id, "}"]
	  | wrap (id::rest) = Symbol.name id :: "," :: wrap rest
     in Symbol.tycSymbol (concat("{" :: wrap labels))
    end

(* mkRECORDtyc : label list -> tycon *)
(* this is an optimization to make similar record tycs point to the same thing,
	thus speeding equality testing on them *)
fun mkRECORDtyc labels =
    let val recordName = labelsToSymbol labels
        val number = Symbol.number recordName
        val name = Symbol.name recordName
     in tyconMap(number,name)
	handle New =>
	  let val tycon = RECORDtyc labels
	   in tyconAdd((number,name),tycon);
	      tycon
	  end
    end

(* numlabels : int -> label list *)
fun numlabels n =
    let fun labels (0,acc) = acc
	  | labels (i,acc) = labels (i-1, NL.numericLabel i :: acc)
    in labels (n,nil)
    end

fun mkTUPLEtyc n =
    case TyconArray.sub(tupleTycons,n)
      of NONE =>
           let val tycon = mkRECORDtyc(numlabels n)
	    in TyconArray.update(tupleTycons,n,SOME tycon);
	       tycon
	   end
       | SOME tycon => tycon

fun isTUPLEtyc(RECORDtyc labels) = NL.checkTupleLabels labels
  | isTUPLEtyc _ = false

(* mkTUPLEtype : ty list -> ty *)
fun mkTUPLEtype (elemTys) = CONty(mkTUPLEtyc (length elemTys), elemTys)

end (* top local *)
end (* structure Tuples *)
