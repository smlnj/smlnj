(* compile.sig
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Trimmed to contain only compile-related stuff but no linking or execution.
 *   -- 07/18/2001 (blume)
 *
 * [DBM, 2022.10.4] Simplified CompInto type (no longer a type constructor)
 *)

signature COMPILE0 =
  sig

    type pickle				(* pickled format *)
    type hash				(* environment hash id *)
    type pid = PersStamps.persstamp
    type guid

    val mkCompInfo : Source.source -> CompInfo.compInfo

    (** take ast, do semantic checks,
     ** then output the new env, absyn and pickles *)
    val elaborate : {
            ast: Ast.dec,
            statenv: StaticEnv.staticEnv,
            compInfo: CompInfo.compInfo
          } -> Absyn.dec * StaticEnv.staticEnv

    (** elaborate as above, then keep on to compile into the binary code *)
    val compile : {
            source: Source.source,
            ast: Ast.dec,
            statenv: StaticEnv.staticEnv,
            compInfo: CompInfo.compInfo,
            guid: guid,
            checkErr: string -> unit
          } -> {
            csegments: CodeObj.csegments,
            newstatenv: StaticEnv.staticEnv,
            absyn: Absyn.dec (* for pretty printing only *),
            exportPid: pid option,
            exportLvars: LambdaVar.lvar list,
            staticPid: hash,
            pickle: pickle,
            imports: ImportTree.import list
          }

  end (* signature COMPILE0 *)

signature COMPILE = COMPILE0 where type pickle = Word8Vector.vector
                               and type hash = PersStamps.persstamp
			       and type guid = string

signature TOP_COMPILE = COMPILE0 where type hash = unit and type guid = unit
