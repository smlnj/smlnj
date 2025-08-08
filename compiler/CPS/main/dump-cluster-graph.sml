(* dump-cluster-graph.sml
 *
 * Dump the cluster graph as a dot file.
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure DumpClusterGraph : sig

    type cluster = CPS.function list

    (* dump the cluster graph to a ".dot" file.  The output-file name is
     * constructed by stripping the ".sml" (or ".sig" or ".fun") extension
     * from `file`, appending the suffix, and adding the ".dot" extension.
     * For example,
     *
     * ```
     * dumpWithSiffix {file="foo.sml", suffix="bar", clusters=clusters}
     * ```
     *
     * will output to a file named "foo-bar.dot".
     *)
    val dumpWithSuffix : {
            file : string,
            suffix : string,
            clusters : cluster list
          } -> unit

    (* `dump(file, cl)` is the same as the expression
     * ```
     * dumpWithSiffix {file="file, suffix="", clusters=cl}
     * ```
     *)
    val dump : string * cluster list -> unit

  end = struct

    structure C = CPS
    structure LV = LambdaVar

    type cluster = C.function list

    (* determine the shape of a graph node *)
    fun nodeShape C.CONT = "\"doubleoctagon\""
      | nodeShape C.KNOWN = "\"ellipse\""
      | nodeShape C.KNOWN_REC = "\"ellipse\""
      | nodeShape C.KNOWN_CHECK = "\"ellipse\""
      | nodeShape C.KNOWN_TAIL = "\"ellipse\""
      | nodeShape C.KNOWN_CONT = "\"octagon\""
      | nodeShape C.ESCAPE = "\"doublecircle\""
      | nodeShape C.NO_INLINE_INTO = raise Fail "unexpected NO_INLINE_INTO"

    fun nodeName v = concat["\"", LV.lvarName v, "\""]

    fun dump' (outS, stem, clusters) = let
          val graphName = concat ["\"", stem, "\""]
          val clusterName = let
                val n = ref 0
                in
                  fn () => let
                      val id = !n
                      in
                        n := id+1; "cluster_"^Int.toString id
                      end
                end
          fun say s = TextIO.output(outS, s)
          fun sayl sl = List.app say sl
          fun sayIndent 0 = ()
            | sayIndent n = (say "  "; sayIndent(n-1))
          fun dumpNode n (fk, f, _, _, _) = (
                sayIndent n; sayl [nodeName f, " [shape=", nodeShape fk, "];\n"])
          fun dumpFn n (fk, f, _, _, e) = let
                val name = nodeName f
                fun walk (C.RECORD(_, _, _, e)) = walk e
                  | walk (C.SELECT(_, _, _, _, e)) = walk e
                  | walk (C.OFFSET _) = raise Fail "deprecated OFFSET"
                  | walk (C.APP(C.LABEL g, _)) = (
                      sayIndent n; sayl [name, " -> ", nodeName g, ";\n"])
                  | walk (C.APP _) = ()
                  | walk (C.FIX _) = raise Fail "unexpected FIX in clusters"
                  | walk (C.SWITCH(_, _, es)) = List.app walk es
                  | walk (C.BRANCH(_, _, _, e1, e2)) = (walk e1; walk e2)
                  | walk (C.SETTER(_, _, e)) = walk e
                  | walk (C.LOOKER(_, _, _, _, e)) = walk e
                  | walk (C.ARITH(_, _, _, _, e)) = walk e
                  | walk (C.PURE(_, _, _, _, e)) = walk e
                  | walk (C.RCC(_, _, _, _, _, e)) = walk e
                in
                  walk e
                end
          (* output the node decls for the cluster *)
          fun dumpClusterNodes n fns = List.app (dumpNode n) fns
          (* output the edges for the cluster *)
          fun dumpClusterEdges n fns = List.app (dumpFn n) fns
          fun dumpCluster fns = (
                sayIndent 1; sayl ["subgraph ", clusterName(), " {\n"];
                dumpClusterNodes 2 fns;
                sayIndent 1; say "}\n")
          in
            sayl ["strict digraph ", graphName, " {\n"];
            sayIndent 1; say "graph [ clusterrank=local ];\n";
            case clusters
             of [cluster] =>
                  List.app (fn func => (dumpNode 1 func; dumpFn 1 func)) cluster
              | _ => (
                  (* first dump the cluster node groups *)
                  List.app dumpCluster clusters;
                  (* then dump the cluster edges *)
                  List.app (dumpClusterEdges 2) clusters)
            (* end case *);
            say "}\n"
          end

    fun dumpWithSuffix {file, suffix, clusters} = let
          val {dir, file} = OS.Path.splitDirFile file
          (* strip the "sml" (or similar) extension *)
          val stem = (case OS.Path.splitBaseExt file
                 of {base, ext=SOME "fun"} => base
                  | {base, ext=SOME "sig"} => base
                  | {base, ext=SOME "sml"} => base
                  | _ => file
                (* end case *))
          val stem = if (suffix <> "") then concat[stem, "-", suffix] else stem
          val outFile = OS.Path.joinDirFile {
                  dir = dir,
                  file = OS.Path.joinBaseExt {base=stem, ext=SOME "dot"}
                }
          val outS = TextIO.openOut outFile
          fun cleanup () = TextIO.closeOut outS
          in
            dump' (outS, stem, clusters)
              handle ex => (cleanup(); raise ex);
            cleanup()
          end

    fun dump (file, clusters) = dumpWithSuffix {file=file, suffix="", clusters=clusters}

  end
