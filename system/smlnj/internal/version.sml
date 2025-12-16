(* version.sml
 *
 * !!! DO NOT EDIT --- GENERATED FROM version.template !!!
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
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

    (* encode the version number and suffix as a string *)
    val toString : {version_id : int list, suffix : string} -> string
    (* decode a string into version number and suffix *)
    val fromString : string -> {version_id : int list, suffix : string} option

  end = struct

    val size = Int.toString(SMLofNJ.SysInfo.getArchSize())

    (* use buildDate (i.e., boot time) if no release date *)
    val releaseDate = (case "December 15, 2025"
           of "" => Date.toString (Date.fromTimeLocal (Time.now ()))
            | d => d
          (* end case *))

    val version = {
	    system = "Standard ML of New Jersey",
	    version_id = [2025, 3],
	    suffix = "rc1",
	    releaseDate = releaseDate
          }

    fun toString {version_id, suffix} = let
	  val vn = String.concatWithMap "." Int.toString version_id
	  in
	    if suffix = "" then vn else concat[vn, "-", suffix]
	  end

    fun fromString s = let
          fun decodeId (versId, suffix) = let
                fun decode ([], []) = NONE
                  | decode ([], ids) = SOME{version_id = List.rev ids, suffix=suffix}
                  | decode (n::ns, ids) = (case Int.fromString n
                       of SOME id => if (id >= 0)
                            then decode (ns, id::ids)
                            else NONE
                        | NONE => NONE
                      (* end case *))
                in
                  decode (String.fields (fn #"." => true | _ => false) versId, [])
                end
          in
            case String.fields (fn #"-" => true | _ => false) s
             of [versId, suffix] => decodeId (versId, suffix)
              | [versId] => decodeId (versId, "")
              | _ => NONE
            (* end case *)
          end

    val version' = toString {version_id = #version_id version, suffix = #suffix version}

    val banner = concat [
	    #system version,
            " [Version ", version', "; ", size, "-bit; ",
	    releaseDate, "]"
	  ]

  end
