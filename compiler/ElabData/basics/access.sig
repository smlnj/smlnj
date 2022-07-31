(* access.sig
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature ACCESS =
  sig

  (* How to find the dynamic value corresponding to a variable.
   * -- LVAR is just a lambda-bound variable --- a "local" varialbe used to
   *    denote a binding in the current compilation unit.
   * -- EXTERN refers to a binding defined externally (in other modules).
   * -- PATH is an address relative a lambda-bound variable or external binding
   *    we find the value of the lambda-bound variable or external, and
   *    then do successive slot selects from that.
   *    PATH's are kept in reverse order.
   * -- NO_ACCESS is used to denote built-in structures that do not
   *    have corresponding dynamic objects (e.g., the built-in Inline is a
   *    structure that declares all the built-in primitives --- it is likely
   *    that NO_ACCESS will go away in the future once we have cleaned up the
   *    bootstrap procedure.
   *)
    datatype access
      = LVAR of LambdaVar.lvar
      | EXTERN of PersStamps.persstamp
      | PATH of access * int
      | NO_ACCESS

  (* How to decide the data representations for data constructors.
   * All true datatypes are divided into four categories, depending on the
   * pair of parameters (m,n) where m is the number of constant constructors
   * and n is the number of value carrying constructors. REF, EXN, SUSP
   * are special constructors for reference cells, exceptions, and suspensions;
   * treating them as data constructors simplifies the match compilation.
   * LISTCONS and LISTNIL are special conreps for unrolled lists. The process
   * of assigning conreps probably should be performed on the intermediate
   * language instead.
   *)
    datatype conrep
      = UNTAGGED                        (* a pointer *)
      | TAGGED of int                   (* a pointer; 1st field is the tag *)
      | TRANSPARENT                     (* singleton dcon datatype *)
      | CONSTANT of int                 (* should be Int.int *)
      | REF
      | EXN of access
      | SUSP of (access * access) option
      | LISTCONS
      | LISTNIL

    datatype consig
      = CSIG of int * int
      | CNIL

  (** for printing the access *)
    val prAcc   : access -> string
  (** for printing the conrep *)
    val prRep   : conrep -> string
  (** for printing the data sign *)
    val prCsig  : consig -> string
  (** testing if a conrep is an exception or not *)
    val isExn   : conrep -> bool

  (** fetching a component out of a structure access *)
    val selAcc  : access * int -> access
  (** duplicating an access variable *)
    val dupAcc  : LambdaVar.lvar * (Symbol.symbol option -> LambdaVar.lvar) -> access

    val namedAcc : Symbol.symbol * (Symbol.symbol option -> LambdaVar.lvar)
		     -> access

    val newAcc  : (Symbol.symbol option -> LambdaVar.lvar) -> access

    val extAcc  : PersStamps.persstamp -> access
    val nullAcc : access

    val accLvar : access -> LambdaVar.lvar option

  end (* signature ACCESS *)
