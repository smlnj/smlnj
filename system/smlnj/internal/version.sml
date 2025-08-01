(* version.sml
 *
 * !!! DO NOT EDIT --- GENERATED FROM version.template !!!
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure SMLNJVersion : sig

    val version : {
            system : string,      	(* the system title *)
	    version_id : int list,	(* the version number *)
	    suffix : string,		(* optional suffix; e.g., "rc1" *)
	    releaseDate : string	(* release date of version *)
	  }

    (* should match the contents of config/version; formed from the version_id and
     * optional suffix.
     *)
    val version' : string

    val banner : string

  end = struct

    val size = Int.toString(SMLofNJ.SysInfo.getArchSize())

    (* use buildDate (i.e., boot time) if no release date *)
    val releaseDate = (case "July 29, 2025"
           of "" => Date.toString (Date.fromTimeLocal (Time.now ()))
            | d => d
          (* end case *))

    val version = {
	    system = "Standard ML of New Jersey",
	    version_id = [2025, 2],
	    suffix = "",
	    releaseDate = releaseDate
          }

    val version' = let
	  val vn = String.concatWithMap "." Int.toString (#version_id version)
	  in
	    if #suffix version = "" then vn else concat[vn, "-", #suffix version]
	  end

    val banner = concat [
	    #system version,
            " [Version ", version', "; ", size, "-bit; ",
	    releaseDate, "]"
	  ]

  end
