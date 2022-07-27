(* predict.sml
 *
 * COPYRIGHT (c) 2006
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (http://www.cs.uchicago.edu/~adrassi)
 * All rights reserved.
 *
 * Decision trees for predictions.
 *)

structure Predict = 
  struct

    structure S = LLKSpec
    structure TSet = Token.Set

    datatype 'a tree
      = Pick of 'a
      | ByTok of (TSet.set * 'a tree) list
      | Choice of 'a list	(* try each in order *)
(*      | Error of TSet.set *)

    datatype predict_maps = PMaps of {
	prodPredict : S.nonterm -> S.prod tree,
	ebnfPredict : S.nonterm -> bool tree
      }

    fun toString aToS tree = let
          fun tos (i, Pick a) = "PICK " ^ (aToS a)
	    | tos (i, ByTok branches) = "\n" ^ i ^
	        (String.concatWith ("\n" ^ i)
		   (map (fn (s, t) => (Token.setToString s) ^ " => " ^
				      tos (i ^ "  ", t)) branches))
          in
            tos ("", tree)
          end

  end
