(* mltree-basis.sml
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies
 *)

structure MLTreeBasis : MLTREE_BASIS =
struct

  type attribs = word

  type misc_op = {name:string, hash:word, attribs:attribs ref}

  datatype cond = LT | LTU | LE | LEU | EQ | NE | GE | GEU | GT | GTU 
                | SETCC 
                | MISC_COND of {name:string, hash:word, attribs:attribs ref}

(* Floating-point conditions: see mltree-basis.sig for documentation *)
  datatype fcond
    = == | ?<> | > | >= | < | <= | ? | <> | <=>
    | ?> | ?>= | ?< | ?<= | ?=
    | SETFCC
    | MISC_FCOND of {name:string, hash:word, attribs:word ref}

  datatype ext = SIGN_EXTEND | ZERO_EXTEND

  datatype rounding_mode = TO_NEAREST | TO_NEGINF | TO_POSINF | TO_ZERO

  datatype div_rounding_mode = DIV_TO_NEGINF | DIV_TO_ZERO

  fun error(msg, oper) = MLRiscErrorMsg.error("MLTreeBasis",msg^": "^oper)

  nonfix <> < > >= <=

  (* These should be datatypes, but FLINT does not optimize them well *)
  type ty = int
  type fty = int

  fun condToString cond =
      case cond of
        LT  => "LT" | LTU => "LTU" | LE  => "LE" | LEU => "LEU"
      | EQ  => "EQ" | NE  => "NE"  | GE  => "GE" | GEU => "GEU"
      | GT  => "GT" | GTU => "GTU"
      | SETCC => "SETCC"
      | MISC_COND{name,...} => name

  fun fcondToString fcond = case fcond
     of ==  => "==" | ?<> => "?<>"
      | > => ">"    | >= => ">="   | <   => "<"  | <=  => "<="
      | ? => "?"    | <>  => "<>"  | <=> => "<=>"
      | ?> => "?<"  | ?>= => "?>=" | ?< => "?<"  | ?<= => "?<=" | ?= => "?="
      | SETFCC => "SETFCC"
      | MISC_FCOND{name, ...} => name

  fun swapCond cond =
      case cond of
        LT  => GT | LTU => GTU | LE  => GE | LEU => GEU | EQ  => EQ 
      | NE  => NE | GE  => LE | GEU => LEU | GT  => LT | GTU => LTU
      | cond => error("swapCond",condToString cond)

(* swap order of arguments *)
  fun swapFcond fcond =
      case fcond of
        ?     => ?   | ==    => ==
      | ?=    => ?=
      | <     => >   | ?<    => ?>
      | <=    => >=  | ?<=   => ?>=
      | >     => <
      | ?>    => ?<
      | >=    => <=  | ?>=   => ?<=
      | <>    => <>
      | <=>   => <=> | ?<>   => ?<>
      | fcond => error("swapFcond",fcondToString fcond)

  fun negateCond cond =
      case cond of
        LT  => GE | LTU => GEU | LE  => GT | LEU => GTU | EQ  => NE
      | NE  => EQ | GE  => LT | GEU => LTU | GT  => LE | GTU => LEU
      | cond => error("negateCond",condToString cond)

  fun negateFcond fcond =
      case fcond of
        ==   => ?<> | ?<>  => ==   | ?    => <=>
      | <=>  => ?  | >    => ?<=  | >=   => ?<
      | ?>   => <= | ?>=  => <    | <    => ?>=
      | <=   => ?> | ?<   => >=   | ?<=  => >
      | <>   => ?= | ?=   => <>
      | _    => error("negateFcond", fcondToString fcond)

  fun hashCond cond =
      case cond of
        LT  => 0w123 | LTU => 0w758 | LE  => 0w81823 | LEU => 0w1231
      | EQ  => 0w987 | NE  => 0w8819 | GE  => 0w88123 | GEU => 0w975
      | GT  => 0w1287 | GTU => 0w2457
      | SETCC => 0w23
      | MISC_COND{hash, ...} => hash

  fun hashFcond fcond =
      case fcond of
        ?     => 0w123 | ==    => 0w12345 | ?=    => 0w123456
      | <   => 0w23456 | ?<    => 0w345
      | <=  => 0w456   | ?<=   => 0w4567
      | >  => 0w5678  | ?>    => 0w56789
      | >=    => 0w67890 | ?>=   => 0w789
      | <>    => 0w890
      | <=>   => 0w991 | ?<>   => 0w391
      | SETFCC => 0w94
      | MISC_FCOND{hash, ...} => hash
  
  fun hashRoundingMode m =
      case m of
        TO_NEAREST => 0w1 | TO_NEGINF => 0w10 
      | TO_POSINF => 0w100 | TO_ZERO     => 0w1000

  fun roundingModeToString m =
      case m of
        TO_NEAREST  => "TO_NEAREST" | TO_NEGINF   => "TO_NEGINF"
      | TO_POSINF   => "TO_POSINF" | TO_ZERO     => "TO_ZERO"
 
end (* MLTreeBasis *)
