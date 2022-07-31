(* os-io-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * The CML version of the generic low-level I/O interface.
 *)

signature CML_OS_IO =
  sig
    eqtype iodesc
	(* an iodesc is an abstract descriptor for an OS object that
	 * supports I/O (e.g., file, tty device, socket, ...).
	 *)
    eqtype iodesc_kind

    val hash : iodesc -> word
	(* return a hash value for the I/O descriptor. *)

    val compare : (iodesc * iodesc) -> order
	(* compare two I/O descriptors *)

    val kind : iodesc -> iodesc_kind
	(* return the kind of I/O descriptor. *)

    structure Kind : sig
	val file : iodesc_kind
	val dir : iodesc_kind 
	val symlink : iodesc_kind 
	val tty : iodesc_kind 
	val pipe : iodesc_kind 
	val socket : iodesc_kind 
	val device : iodesc_kind 
      end

    type poll_desc
	(* this is an abstract representation of a polling operation on
	 * an I/O descriptor.
	 *)
    type poll_info
	(* this is an abstract representation of the per-descriptor
	 * information returned by the poll operation.
	 *)

    val pollDesc : iodesc -> poll_desc option
	(* create a polling operation on the given descriptor; note that
	 * not all I/O devices support polling.
	 *)
    val pollToIODesc : poll_desc -> iodesc
	(* return the I/O descriptor that is being polled *)

    exception Poll

  (* set polling events; if the polling operation is not appropriate
   * for the underlying I/O device, then the Poll exception is raised.
   *)
    val pollIn  : poll_desc -> poll_desc
    val pollOut : poll_desc -> poll_desc
    val pollPri : poll_desc -> poll_desc

  (* polling functions *)
    val poll : (poll_desc list * Time.time option) -> poll_info list
	(* a timeout of NONE means wait indefinitely; a timeout of
	 * (SOME Time.zeroTime) means do not block.
	 *)
    val pollEvt : poll_desc list -> poll_info list Event.event

  (* check for conditions *)
    val isIn 		: poll_info -> bool
    val isOut		: poll_info -> bool
    val isPri		: poll_info -> bool
    val infoToPollDesc  : poll_info -> poll_desc

  end (* OS_IO *)

