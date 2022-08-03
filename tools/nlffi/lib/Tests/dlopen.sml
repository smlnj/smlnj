structure DL = struct
    local
	structure CI = Unsafe.CInterface
    in
	val dlopen : string option * bool * bool -> Word32.word =
	    CI.c_function "UNIX-Dynload" "dlopen"
	val dlsym : Word32.word * string -> Word32.word =
	    CI.c_function "UNIX-Dynload" "dlsym"
        val dlerror : unit -> string option =
	    CI.c_function "UNIX-Dynload" "dlerror"
	val dlclose : Word32.word -> unit =
	    CI.c_function "UNIX-Dynload" "dlclose"
    end
end
