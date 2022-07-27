(* mkfrags.sml
 *
 * Program to generate a file "fragments.sml" containing a fragments structure
 * from a CATALOG file.  A CATALOG file has the following layout
 *
 *      <structure name>
 *      <input file>    <fragment name>
 *      <input file>    <fragment name>
 *      ...
 *      <input file>    <fragment name>
 *
 * The resulting file (named fragments.sml) will contain a structure with the given
 * name; the structure consists of named fragments.  Two kinds of input files are
 * supported.  If the input file has a ".in" suffix, then it is converted to an
 * SML literal string in the output file.  If it has a ".json" suffix, then it
 * is parsed as a JSON value (see the SML/NJ JSON library) and the resulting
 * value in the output will be SML code that defines the corresponding JSON.value
 * value.
 *
 * COPYRIGHT (c) 2017 John Reppy (http://cs.uchicago.edu/~jhr)
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * This code is part of the SML Compiler Utilities, which can be found at
 *
 *      https://github.com/JohnReppy/sml-compiler-utils
 *
 * This file has been specialized for the ASDL component of the SML/NJ system.
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure MkFrags : sig

  (* `mkFragments dir` generates the file `dir/fragments.sml` using the catalog
   * file `dir/fragments/CATALOG`.
   *)
    val mkFragments : string -> unit

  (* `mkMakefile dir` generates the file `dir/fragments.gmk` using the catalog
   * file `dir/fragments/CATALOG`.
   *)
    val mkMakefile : string -> unit

  end = struct

    structure F = Format

  (* load the catalog from the file *)
    fun loadCatalog file = let
          val inS = TextIO.openIn file
        (* report a bogus input line *)
          fun error (lnum, ln) = raise Fail (concat[
                  "[", file, ":", Int.toString lnum, "] bogus input: \"",
                  String.toString ln, "\""
                ])
        (* get the structure name *)
          val structName = (case TextIO.inputLine inS
                 of NONE => raise Fail "empty CATALOG file"
                  | SOME ln => (case String.tokens Char.isSpace ln
                       of [name] => name
                        | _ => error (1, ln)
                      (* end case *))
                (* end case *))
          fun lp (lnum, l) = (case TextIO.inputLine inS
                 of NONE => List.rev l
                  | SOME ln => (case String.tokens Char.isSpace ln
                     of [] => lp(lnum+1, l)
                      | s1::sr => if String.isPrefix "#" s1
                          then lp(lnum+1, l)
                          else (case sr
                             of [s2] => lp (lnum+1, (s1, s2) :: l)
                              | _ => error (lnum, ln)
                            (* end case *))
                    (* end case *))
                (* end case *))
          in
            (structName, lp(2, []) before TextIO.closeIn inS)
              handle ex => (TextIO.closeIn inS; raise ex)
          end

  (* header for the generated SML file *)
    val smlHead = "\
          \(* %s\n\
          \ *\n\
          \ * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (https://smlnj.org)\n\
          \ * All rights reserved.\n\
          \ *\n\
          \ * !!! THIS FILE WAS GENERATED; DO NOT EDIT !!!\n\
          \ *)\n\
          \\n\
          \structure %s =\n\
          \  struct\n\
          \"

  (* footer for the generated SML file *)
    val smlFoot = "\
          \\n\
          \  end\n\
          \"

  (* load the contents of an ".in" file *)
    fun load srcFile = let
          val inS = TextIO.openIn srcFile
          fun lp l = (case TextIO.inputLine inS
                 of NONE => List.rev l
                  | SOME ln => lp(ln::l)
                (* end case *))
          in
            (lp [] before TextIO.closeIn inS)
              handle ex => (TextIO.closeIn inS; raise ex)
          end

    fun doInFile (outS, fragDir) (srcFile, smlVar) = let
          val text = load (OS.Path.concat (fragDir, srcFile))
          fun prf (fmt, items) = TextIO.output(outS, F.format fmt items)
          in
            prf ("\n", []);
            prf ("    val %s = \"\\\n", [F.STR smlVar]);
            prf ("          \\(*---------- begin %s ----------*)\\n\\\n", [F.STR srcFile]);
            List.app (fn ln => prf("          \\%s\\\n", [F.STR(String.toString ln)])) text;
            prf ("          \\(*---------- end %s ----------*)\\n\\\n", [F.STR srcFile]);
            prf ("          \\\"\n", [])
          end

    fun doFile arg = let
          val doInFile = doInFile arg
          in
            fn (srcFile, smlVar) => (case OS.Path.ext srcFile
                 of SOME "in" => doInFile (srcFile, smlVar)
                  | _ => raise Fail "unexpected/missing file suffix"
                (* end case *))
          end

    fun mkFragments dir = let
          val fragDir = OS.Path.concat(dir, "fragments")
          val catalogFile = OS.Path.concat(fragDir, "CATALOG")
          val fragFile = OS.Path.concat(dir, "fragments.sml")
          val (structName, catalog) = if OS.FileSys.access(catalogFile, [OS.FileSys.A_READ])
                then loadCatalog catalogFile
                else raise Fail(concat["cannot find \"", catalogFile, "\""])
          val outS = TextIO.openOut fragFile
          fun prf (fmt, items) = TextIO.output(outS, F.format fmt items)
          in
            prf (smlHead, [F.STR(OS.Path.file fragFile), F.STR structName]);
            List.app (doFile (outS, fragDir)) catalog;
            prf (smlFoot, []);
            TextIO.closeOut outS
          end

  (* header for the generated Makefile file *)
    val mkHead = "\
          \# fragments.gmk\n\
          \#\n\
          \# COPYRIGHT (c) 2018 The Fellowship of SML/NJ (https://smlnj.org)\n\
          \# All rights reserved.\n\
          \#\n\
          \# !!! THIS FILE WAS GENERATED; DO NOT EDIT !!!\n\
          \#\n\
          \\n\
          \"

    fun mkMakefile dir = let
          val fragDir = OS.Path.concat(dir, "fragments")
          val catalogFile = OS.Path.concat(fragDir, "CATALOG")
          val makefile = OS.Path.concat(dir, "fragments.gmk")
          val (_, catalog) = if OS.FileSys.access(catalogFile, [OS.FileSys.A_READ])
                then loadCatalog catalogFile
                else raise Fail(concat["cannot find \"", catalogFile, "\""])
          val outS = TextIO.openOut makefile
          fun prf (fmt, items) = TextIO.output(outS, F.format fmt items)
          fun prDep file = prf(" \\\n    %s/fragments/%s", [F.STR dir, F.STR file])
          in
            prf (mkHead, []);
            prf ("%s/fragments.sml:", [F.STR dir]);
            prDep "CATALOG";
            List.app (fn (srcFile, _) => prDep srcFile) catalog;
            prf ("\n", []);
            TextIO.closeOut outS
          end

  end
