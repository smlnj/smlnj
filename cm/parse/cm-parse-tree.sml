(* cm-parse-tree.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * Parse-tree representation of CM specifications.
 *)

structure CMParseTree =
  struct

    (* to mark positions in files *)
    type srcpos = int  (* character position from beginning of stream (base 0) *)
    type region = srcpos * srcpos   (* start and end position of region *)

    (* preprocessor conditionals *)
    datatype 'a guard
      = MarkGuard of 'a guard * region
      | Guard of bexp * 'a list       (* '#if' or '#elif' *)

    type 'a conditional = 'a guard list * 'a list

    datatype namespace = SIG | FUNSIG | FUN | STR | CM

    (* boolean expressions *)
    datatype bexp
      = MarkBExp of bexp * region
      | OrElseBExp of bexp * bexp
      | AndAlsoBExp of bexp * bexp
      | EqBExp of bexp * bexp
      | NEqBExp of bexp * bexp
      | NotBExp of bexp
      | DefinedBExp of namespace * string
      | CmpBExp of aexp * cmpop * aexp

    and cmpop = EqOp | NEqOp | LtOp | LteOp | GtOp | GteOp

    (* arithmetic expressions *)
    and aexp
      | MarkAExp of aexp * region
      | BinAExp of aexp * binop * aexp
      | NegAExp of aexp
      | NumAExp of IntInf.int
      | IdAExp of string

    and binop = AddOp | SubOp | MulOp | DivOp | ModOp

    datatype group
      = MarkGroup of group * region
      | Group of {
            priv : priv_spec list,
            owner : string option,
            exports : export list,
            members : member list
          }
      | Library of {
            priv : priv_spec list,
            version : string option,
            exports : export list,
            members : member list
          }

    and priv_spec
      = MarkPriv of priv_spec * region
      | Priv of string
      | WrappedPriv of string list      (* only in libraries *)

    and export
      = MarkExport of export * region

    and member
      = MarkMemb of member * region
      | PathMemb of pathname * string option * tool_opts option
      | CondMemb of member conditional
      | ErrorMemb of string

    and tool_opts
      = MarkToolOpts of tool_opts * region
      | ToolOpts of ??

    (* `(path, isNative, rgn)` *)
    withtype pathname = string * bool * region

  end
