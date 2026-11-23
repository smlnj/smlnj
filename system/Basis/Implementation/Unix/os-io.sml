(* os-io.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * NOTE: this interface has been proposed, but not yet adopted by the
 * Standard basis committee.
 *)

local
  structure Word = WordImp
  structure Int32 = Int32Imp
  structure Int = IntImp
  structure SysWord = SysWordImp
in
structure OS_IO : OS_IO =
  struct

  (* an iodesc is an abstract descriptor for an OS object that
   * supports I/O (e.g., file, tty device, socket, ...).
   *)
    type iodesc = OS.IO.iodesc

    datatype iodesc_kind = K of string

  (* return a hash value for the I/O descriptor. *)
    fun hash (OS.IO.IODesc fd) = Word.fromInt fd

  (* compare two I/O descriptors *)
    fun compare (OS.IO.IODesc fd1, OS.IO.IODesc fd2) = Int.compare(fd1, fd2)

    structure Kind =
      struct
	val file = K "FILE"
	val dir = K "DIR"
	val symlink = K "LINK"
	val tty = K "TTY"
	val pipe = K "PIPE"
	val socket = K "SOCK"
	val device = K "DEV"
      end

  (* return the kind of I/O descriptor *)
    fun kind (OS.IO.IODesc fd) = let
	  val fd = Posix.FileSys.wordToFD(SysWord.fromInt fd)
	  val stat = Posix.FileSys.fstat fd
	  in
	    if      (Posix.FileSys.ST.isReg stat) then Kind.file
	    else if (Posix.FileSys.ST.isDir stat) then Kind.dir
	    else if (Posix.FileSys.ST.isChr stat) then Kind.tty
	    else if (Posix.FileSys.ST.isBlk stat) then Kind.device (* ?? *)
	    else if (Posix.FileSys.ST.isLink stat) then Kind.symlink
	    else if (Posix.FileSys.ST.isFIFO stat) then Kind.pipe
	    else if (Posix.FileSys.ST.isSock stat) then Kind.socket
	    else K "UNKNOWN"
	  end

    type poll_flags = {rd : bool, wr : bool, pri : bool}
    datatype poll_desc = PollDesc of (iodesc * poll_flags)
    datatype poll_info = PollInfo of (iodesc * poll_flags)

  (* create a polling operation on the given descriptor; note that
   * not all I/O devices support polling, but for the time being, we
   * don't test for this.
   *)
    fun pollDesc iod = SOME(PollDesc(iod, {rd=false, wr=false, pri=false}))

  (* return the I/O descriptor that is being polled *)
    fun pollToIODesc (PollDesc(iod, _)) = iod

    exception Poll

  (* set polling events; if the polling operation is not appropriate
   * for the underlying I/O device, then the Poll exception is raised.
   *)
    fun pollIn (PollDesc(iod, {rd, wr, pri})) =
	  PollDesc(iod, {rd=true, wr=wr, pri=pri})
    fun pollOut (PollDesc(iod, {rd, wr, pri})) =
	  PollDesc(iod, {rd=rd, wr=true, pri=pri})
    fun pollPri (PollDesc(iod, {rd, wr, pri})) =
	  PollDesc(iod, {rd=rd, wr=wr, pri=true})

  (* polling function *)
    local
(* TODO: pass a `Time.time option` argument for the timeout argument *)
      val poll' : ((int * word ref) list * (Int32.int * int) option) -> bool =
	    CInterface.c_function "POSIX-OS" "poll"
      fun join (false, _, w) = w
        | join (true, b, w) = Word.orb(w, b)
      fun test (w, b) = (Word.andb(w, b) <> 0w0)
      val rdBit = 0w1 and wrBit = 0w2 and priBit = 0w4
      fun fromPollDesc (PollDesc(OS.IO.IODesc fd, {rd, wr, pri})) =
	    ( fd,
	      join (rd, rdBit, join (wr, wrBit, join (pri, priBit, 0w0)))
	    )
      fun toPollInfo (fd, w) = PollInfo(OS.IO.IODesc fd, {
	      rd = test(w, rdBit), wr = test(w, wrBit), pri = test(w, priBit)
	    })
      (* given a list `pds` of poll descriptors that might contain duplicate
       * file descriptors, we return a pair `(pds', npds)` of two lists:
       *      pds'    -- is a list of triples `(fd, mask, ref flgs)` that corresponds
       *                 the list `pds`.  The `mask` is the bit-flags for the
       *                 corresponding entry in `pds`, and the reference is shared
       *                 between all occurrences of `fd` in `pds'`.
       *      npds    -- a list of pairs `(fd, ref flgs)`, where the `fd` are
       *                 unique and the reference is shared with occurrences
       *                 of `fd` in `pds'`.  The `flgs` value is the merge of
       *                 the flags for `fd` in `pds`.
       *)
      fun normalize pds = let
            fun lp ([], pds', npds) = (List.rev pds', npds)
              | lp (pd :: pdr, pds', npds) = let
                  val (fd, flgs) = fromPollDesc pd
                  (* search the `npds` list for `fd`; if it is not found, then  *)
                  fun find ([], prefix) = let
                        val r = ref flgs
                        in
                          (* add fd to the end of the npds list *)
                          lp (pdr,
                            (fd, flgs, r)::pds',
                            List.revAppend(prefix, [(fd, r)]))
                        end
                    | find (npds' as (fd', r')::npdr', prefix) = if (fd < fd')
                          then let
                            val r = ref flgs
                            in
                              lp (pdr,
                                (fd, flgs, r)::pds',
                                List.revAppend(prefix, (fd, r)::npds'))
                            end
                        else if (fd = fd')
                          then (
                            (* duplicate FD, so merge flags *)
                            r' := Word.orb(!r', flgs);
                            lp (pdr, (fd, flgs, r')::pds', npds))
                          else find (npdr', (fd', r')::prefix)
                  in
                    find (npds, [])
                  end
            in
              lp (pds, [], [])
            end
    (* after the `poll` operation, we need to construct the result list such that
     *
     *   1) the list of return items should be in the same order as the
     *     corresponding list of arguments.
     *      (int * word ref) list * time option -> bool
     *
     *   2) return items should contain no more information than was queried for
     *     (this matters when the same descriptor is covered by multiple items).
     *)
    fun result pds = let
          fun lp ((fd, mask, ref w)::pdr, res) = let
                val flgs' = Word.andb(mask, w)
                in
                  if flgs' = 0w0
                    then lp (pdr, res)
                    else lp (pdr, toPollInfo(fd, flgs')::res)
                end
            | lp (_, res) = List.rev res
          in
            lp (pds, [])
          end
    in
    fun poll ([], _) = []
      | poll (pds, timeOut) = let
	  val timeOut = (case timeOut
		 of SOME t =>
		    let val usec = TimeImp.toMicroseconds t
			val (sec, usec) = IntInfImp.divMod (usec, 1000000)
		    in
			SOME (Int32.fromLarge sec, Int.fromLarge usec)
		    end
		  | NONE => NONE
		(* end case *))
          val (pds', npds) = normalize pds
	  in
            if poll' (npds, timeOut)
              then result pds'
              else []
	  end
    end (* local *)

  (* check for conditions *)
    fun isIn (PollInfo(_, flgs)) = #rd flgs
    fun isOut (PollInfo(_, flgs)) = #wr flgs
    fun isPri (PollInfo(_, flgs)) = #pri flgs
    fun infoToPollDesc  (PollInfo arg) = PollDesc arg

  end (* OS_IO *)
end

