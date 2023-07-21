(* cm/smlfile/dbm/skeleton.sml
 * SML source skeletons.
 *
 *   Copyright (c) 2023 by The Fellowship of SML/NJ
 *
 *   The idea of skeletons is taken from the original SC (where they were
 *   called "decl"s after one of the datatypes involved).
 *   The new definitions used here are a lot simpler than the original ones;
 *   they allow for a more succinct dependency analysis, and are easier to
 *   pickle/unpickle.  Moreover, the ast->skeleton converter has been made
 *   smarter -- resulting in smaller skeletons.
 *
 * Author: Matthias Blume (matthias.blume@gmail.com)
 * Edited: DBM, 2023.7
 *
 * The copyright notices for earlier versions are:
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *   Copyright (c) 1993 by Carnegie Mellon University,
 *                         School of Computer Science
 *                         contact: Gene Rollins (rollins+@cs.cmu.edu)
 *)

structure Skeleton =
struct

local

  structure S = Symbol
  structure SS = SymbolSet

in

  datatype decl
    = Bind of S.symbol * exp
    | Local of decl * decl
    | Par of decl list
    | Seq of decl list
    | Open of S.symbol list   (* open a single path *)
    | Ref of SS.set           (* set not empty? *)

  and exp
    = Var of S.symbol list   (* symbol list (sympath) not null *)
    | Decl of decl list	     (* implicit Seq, decl list not null? *)
    | Let of decl list * exp (* implicit Seq, decl list not null? *)
    | Pair of exp * exp      (* ? or more generally SeqExp of exp list? *)

end (* top local *)
end (* structure Skeleton *)

(* NOTES

* seems to be some redundancy in these declarations of decl and exp types:
    decl list in both decl and exp

* Ref decl constructor seems to record the "free structure/module symbols" of
  the following decls.  Should it always be maintained at the front? Should
  it become a separate "binding" construct?  E.g.

  datatype decl =
    ...
    Free of SS.set * decl(s)

  The idea is presumably to make it easy to identify the ultimate
  free symbols, i.e. the implicitly imported module names.

  If a Ref is added to the front of a decl list with a Ref head, the
  two symsets are coalesced to produce a single head Ref. (join_dl)
  

* Nested binary Pair exp constructions could be flattened to a list.
  Used to represent the components of a structure?  What else?

* The Decl exp constructor may represent structures.

*)
