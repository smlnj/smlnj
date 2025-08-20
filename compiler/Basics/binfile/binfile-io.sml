(* binfile-io.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * Basic I/O support for containerized binfiles (see
 * https://github.com/smlnj/.github/wiki/New-Binfile-Format)
 *)

structure BinfileIO :> BINFILE_IO =
  struct

    structure W = Word

    (* section IDs are represented as words internally, and as four-character
     * little-endian codes externally.
     *)
    structure SectId =
      struct

        eqtype t= Word.word

        val fromString : string -> t

        fun fromString s = (case explode s
               of [c1, c2, c3, c4] =>
                    W.<<(W.fromInt(ord c4), 0w24) +
                    W.<<(W.fromInt(ord c3), 0w16) +
                    W.<<(W.fromInt(ord c2), 0w8) +
                    W.fromInt(ord c1)
                | _ => raise Size
              (* end case *))

        fun toString (id : t) = ??

        val binfile : section_id = sectionID "BINF"
        val info : section_id = sectionID "INFO"
        val import : section_id = sectionID "IMPT"
        val export : section_id = sectionID "EXPT"
        val pids : section_id = sectionID "PIDS"
        val guid : section_id = sectionID "GUID"
        val literals : section_id = sectionID "LITS"
        val code : section_id = sectionID "CODE"
        val cfkPickle : section_id = sectionID "CFGP"
        val staticEnv : section_id = sectionID "SENV"
        val libStamp : section_id = sectionID "STMP"
        val depGraphPickle : section_id = sectionID "PDGR"
        val padding : section_id = sectionID "PAD "

      end

    structure Hdr =
      struct

        datatype kind = BinFile | StableArchive

        type sect_desc = {
            kind : SectId.t,
            flags : word,
            offset : Position.int,
            size : int
          }

        val version = 0wx20250801

        type t = {
            kind : kind,
            version : word,
            smlnjVersion : {id : int list, suffix : string},
            sects : sect_desc vector
          }

      end


    type output = {
        outV : Word64Vector.vector -> unit
      }

    type input = {
      }

  end
