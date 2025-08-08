(* skel-io.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * Reading and writing skeleton pickles to skeleton files.
 *)

signature SKELIO =
  sig
    val read : string * TStamp.t -> Skeleton.decl option
    val write : string * Skeleton.decl * TStamp.t -> unit
  end

structure SkelIO :> SKELIO =
  struct

    structure SK = Skeleton
    structure R = SkeletonRep

    val version = "Skeleton 6\n"

    (* convert from internal skeleton representation to pickle rep *)
    fun toRep dcl = let
          fun symToRep sym = let
                val ns = (case Symbol.nameSpace sym
                       of Symbol.VALspace => R.VAL
                        | Symbol.TYCspace => R.TYC
                        | Symbol.SIGspace => R.SIG
                        | Symbol.STRspace => R.STR
                        | Symbol.FCTspace => R.FCT
                        | Symbol.FIXspace => R.FIX
                        | Symbol.LABspace => R.LAB
                        | Symbol.TYVspace => R.TYV
                        | Symbol.FSIGspace => R.FSIG
                      (* end case *))
                in
                  (ns, Symbol.name sym)
                end
          fun symsToRep syms = List.map symToRep syms
          fun spathToRep (SymPath.SPATH syms) = symsToRep syms
          fun declToRep (SK.Bind(sym, modE)) = R.Bind(symToRep sym, modExpToRep modE)
            | declToRep (SK.Local(dcl1, dcl2)) = R.Local(declToRep dlc1, declToRep dcl2)
            | declToRep (SK.Par dcls) = R.Par(declsToRep dcls)
            | declToRep (SK.Seq dcls) = R.Seq(declsToRep dcls)
            | declToRep (SK.Open modE) = R.Open(modExpToRep modE)
            | declToRep (SK.Ref symSet) = R.Ref(symsToRep (SymbolSet.listItems symSet))
          and declsToRep dcls = List.map declToRep dcls
          and modExpToRep (SK.Var sp) = R.Var(spathToRep sp)
            | modExpToRep (SK.Decl dcls) = R.Decl(declsToRep dcls)
            | modExpToRep (SK.Let(dcls, modE)) =
                R.Let(declsToRep dcls, modExpToRep modE)
            | modExpToRep (SK.Ign1(modE1, modE2)) =
                R.Ign1(modExpToRep modE1, modExpToRep modE2)
          in
            declToRep dcl
          end

    (* convert from pickle rep to the internal skeleton representation *)
    fun fromRep dcl = let
          fun symFromRep ((ns, n) : R.symbol) = (case ns
                 of R.VAL => Symbol.varSymbol n
                  | R.TYC => Symbol.tycSymbol n
                  | R.SIG => Symbol.sigSymbol n
                  | R.STR => Symbol.strSymbol n
                  | R.FCT => Symbol.fctSymbol n
                  | R.FIX => Symbol.fixSymbol n
                  | R.LAB => Symbol.labSymbol n
                  | R.TYV => Symbol.tyvSymbol n
                  | R.FSIG => Symbol.fsigSymbol n
                (* end case *))
          fun symsFromRep syms = List.map symFromRep syms
          fun spathFromRep syms = SymPath.SPATH(symsFromRep syms)
          fun declFromRep (R.Bind(sym, modE)) =
                SK.Bind(symFromRep sym, modExpFromRep modE)
            | declFromRep (R.Local(dcl1, dcl2)) =
                SK.Local(declFromRep dlc1, declFromRep dcl2)
            | declFromRep (R.Par dcls) = SK.Par(declsFromRep dcls)
            | declFromRep (R.Seq dcls) = SK.Seq(declsFromRep dcls)
            | declFromRep (R.Open modE) = SK.Open(modExpFromRep modE)
            | declFromRep (R.Ref syms) = SK.Ref(SymbolSet.fromList(symsFromRep syms))
          and declsFromRep dcls = List.map declFromRep dcls
          and modExpFromRep (R.Var sp) = SK.Var(spathFromRep sp)
            | modExpFromRep (R.Decl dcls) = SK.Decl(declsFromRep dcls)
            | modExpFromRep (R.Let(dcls, modE)) =
                SK.Let(declsFromRep dcls, modExpFromRep modE)
            | modExpFromRep (R.Ign1(modE1, modE2)) =
                SK.Ign1(modExpFromRep modE1, modExpFromRep modE2)
          in
            declToRep dcl
          end

    fun readDecl inS = let
          val vers = Byte.bytesToString(BinIO.inputN(inS, size version))
          in
            if vers = version
              then fromRep(SkeletonRepFilePickle.read_decl inS)
(* FIXME: print an error message *)
              else raise Fail(concat[
                  "SkelIO: incorrect version string \"", String.toString vers,
                  "\", expected \"", String.toString version, "\""
                ])
          end

    fun read (s, ts) = if TStamp.needsUpdate { target = TStamp.fmodTime s, source = ts }
          then NONE
	  else SOME(SafeIO.perform {
              openIt = fn () => BinIO.openIn s,
              closeIt = BinIO.closeIn,
              work = readDecl,
              cleanup = fn _ => ()
            }) handle _ => NONE

    fun writeDecl dcl outS = (
          BinIO.output(outS, Byte.stringToBytes version);
          SkeletonRepFilePickle.write_decl(outS, toRep dcl))

    fun write (s, sk, ts) = let
	  fun cleanup _ = (
	        OS.FileSys.remove s handle _ => ();
	        Say.say ["[writing ", s, " failed]\n"])
          in
            SafeIO.perform {
                openIt = fn () => AutoDir.openBinOut s,
                closeIt = BinIO.closeOut,
                work = writeDecl sk,
                cleanup = cleanup
              };
	    TStamp.setTime (s, ts)
          end

  end
