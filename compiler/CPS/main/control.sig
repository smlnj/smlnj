(* compiler/CPS/main/control.sig *)

(* signature for CPSControl *)

signature CPSCONTROL =
sig

    val closureStrategy : int ref
    val cpsopt : string list ref		(* list of cpsopt phases *)
    val rounds : int ref
    val betacontract : bool ref
    val eta : bool ref
    val selectopt : bool ref
    val dropargs : bool ref
    val deadvars : bool ref
    val flattenargs : bool ref
    val extraflatten : bool ref
    val switchopt : bool ref
    val handlerfold : bool ref
    val branchfold : bool ref
    val arithopt : bool ref
    val betaexpand : bool ref
    val unroll : bool ref
    val invariant: bool ref
    val lambdaprop: bool ref
    val sharepath : bool ref
    val staticprof : bool ref
    val unroll_recur : bool ref
    val debugcps : bool ref
    val bodysize : int ref
    val reducemore : int ref
    val comment : bool ref	(* used in CPS/clos/closure.sml to control debug messages *)
    val knownGen : int ref
    val knownClGen : int ref
    val escapeGen : int ref
    val calleeGen : int ref
    val etasplit : bool ref
    val uncurry : bool ref
    val ifidiom : bool ref
    val comparefold : bool ref
    val debugLits : bool ref
    val newLiterals : bool ref
    val deadup : bool ref
    val printit : bool ref
    val printClusters : bool ref
    val debugSpill : bool ref
    val debugSpillInfo : bool ref

end (* signature CPSCONTROL *)
