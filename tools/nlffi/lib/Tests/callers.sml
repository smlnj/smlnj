val ii_i = p : Word32.word * (Int32.int * Int32.int) *
	       (unit *  int * int -> int) list ->
	       Int32.int
val i_i = p : Word32.word * Int32.int *
	      (unit * int -> int) list ->
	       Int32.int
val r_r = p : Word32.word * real *
	      (unit * real -> real) list ->
	      real
val ir_r = p : Word32.word * (Int32.int * real) *
	       (unit * int * real -> real) list ->
	       real
val ri_r = p : Word32.word * (real * Int32.int) *
	       (unit * real * int -> real) list ->
	       real
val u_u = p : Word32.word * unit *
	      (unit -> unit) list
	      -> unit
val w_p = p : Word32.word * Word32.word *
	      (unit * word -> string) list
	      -> Word32.word
val p_u = p : Word32.word * Word32.word *
	      (unit * string -> unit) list
	      -> unit
val u_i = p : Word32.word * unit * (unit -> int) list -> Int32.int
val i_u = p : Word32.word * Int32.int * (unit * int -> unit) list -> unit
