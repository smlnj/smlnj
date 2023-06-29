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
 *
 * The copyright notices of the earlier versions are:
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *   Copyright (c) 1993 by Carnegie Mellon University,
 *                         School of Computer Science
 *                         contact: Gene Rollins (rollins+@cs.cmu.edu)
 *)

structure Skeleton =
struct

    type symbol = Symbol.symbol
    type sympath = SymPath.path

    datatype decl
      = Bind of symbol * modExp
      | Local of decl * decl
      | Par of decl list
      | Seq of decl list
      | Open of modExp
      | Ref of SymbolSet.set

    and modExp
      = Var of sympath
      | Decl of decl list		(* implicit Seq *)
      | Let of decl list * modExp	(* implicit Seq *)
      | Ign1 of modExp * modExp         (* Ign1 ? *)

end (* structure Skeleton *)

