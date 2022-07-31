(* posix-filesys.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Structure for POSIX 1003.1 file system operations
 *)

local
  structure SysWord = SysWordImp
  structure Word32 = Word32Imp
  structure Int32 = Int32Imp
  structure Word64 = Word64Imp
  structure Int64 = Int64Imp
  structure Position = PositionImp
  structure Time = TimeImp
in
structure POSIX_FileSys =
  struct
    val ++ = SysWord.orb
    val & = SysWord.andb
    infix ++ &

    type s_word = SysWord.word
    type s_int = SysInt.int	(* == Int.int *)

    fun cfun x = CInterface.c_function "POSIX-FileSys" x
    val osval : string -> s_int = cfun "osval"
    val w_osval = SysWord.fromInt o osval

    datatype uid = UID of s_word
    datatype gid = GID of s_word

    datatype file_desc = FD of {fd : s_int}
    fun intOf (FD{fd}) = fd
    fun fd fd = FD{fd=fd}
    fun fdToWord (FD{fd}) = SysWord.fromInt fd
    fun wordToFD fd = FD{fd = SysWord.toInt fd}

  (* conversions between OS.IO.iodesc values and Posix file descriptors. *)
    fun fdToIOD (FD{fd,...}) = OS.IO.IODesc fd
    fun iodToFD (OS.IO.IODesc fd) = SOME(FD{fd = fd})

    val o_rdonly = w_osval "O_RDONLY"
    val o_wronly = w_osval "O_WRONLY"
    val o_rdwr = w_osval "O_RDWR"

    datatype open_mode = O_RDONLY | O_WRONLY | O_RDWR
    fun omodeFromWord omode =
          if omode = o_rdonly then O_RDONLY
          else if omode = o_wronly then O_WRONLY
          else if omode = o_rdwr then O_RDWR
          else raise Fail("POSIX_FileSys.omodeFromWord: unknown mode 0x"^SysWord.toString omode)

    fun omodeToWord O_RDONLY = o_rdonly
      | omodeToWord O_WRONLY = o_wronly
      | omodeToWord O_RDWR = o_rdwr

    fun uidToWord (UID i) = i
    fun wordToUid i = UID i
    fun gidToWord (GID i) = i
    fun wordToGid i = GID i

    type c_dirstream = Assembly.object  (* the underlying C DIRSTREAM *)

    datatype dirstream = DS of {
	dirStrm : c_dirstream,
	isOpen : bool ref
      }

    val opendir' : string -> c_dirstream  = cfun "opendir"
    val readdir' : c_dirstream -> string  = cfun "readdir"
    val rewinddir' : c_dirstream -> unit  = cfun "rewinddir"
    val closedir' : c_dirstream -> unit	  = cfun "closedir"
    fun opendir path = DS{
	    dirStrm = opendir' path,
	    isOpen = ref true
	  }
    fun readdir (DS{dirStrm, isOpen = ref false}) =
	  raise Assembly.SysErr("readdir on closed directory stream", NONE)
      | readdir (DS{dirStrm, ...}) =
	  case readdir' dirStrm of
	      "" => NONE
	    | name => SOME name
    fun rewinddir (DS{dirStrm, isOpen = ref false}) =
	  raise Assembly.SysErr("rewinddir on closed directory stream", NONE)
      | rewinddir (DS{dirStrm, ...}) = rewinddir' dirStrm
    fun closedir (DS{dirStrm, isOpen = ref false}) = ()
      | closedir (DS{dirStrm, isOpen}) = (
	  isOpen := false;
	  closedir' dirStrm)

    val chdir : string -> unit = cfun "chdir"
    val getcwd : unit -> string = cfun "getcwd"

    val stdin  = fd 0
    val stdout = fd 1
    val stderr = fd 2

    structure S =
      struct
        local structure BF = BitFlagsFn ()
	in
	  open BF
	  type mode = flags
	end

        val irwxu = fromWord (w_osval "irwxu")
        val irusr = fromWord (w_osval "irusr")
        val iwusr = fromWord (w_osval "iwusr")
        val ixusr = fromWord (w_osval "ixusr")
        val irwxg = fromWord (w_osval "irwxg")
        val irgrp = fromWord (w_osval "irgrp")
        val iwgrp = fromWord (w_osval "iwgrp")
        val ixgrp = fromWord (w_osval "ixgrp")
        val irwxo = fromWord (w_osval "irwxo")
        val iroth = fromWord (w_osval "iroth")
        val iwoth = fromWord (w_osval "iwoth")
        val ixoth = fromWord (w_osval "ixoth")
        val isuid = fromWord (w_osval "isuid")
        val isgid = fromWord (w_osval "isgid")

      end

    structure O =
      struct
        local structure BF = BitFlagsFn ()
	in
	  open BF
	end

        val append   = fromWord (w_osval "O_APPEND")
        val dsync    = fromWord (w_osval "O_DSYNC")
        val excl     = fromWord (w_osval "O_EXCL")
        val noctty   = fromWord (w_osval "O_NOCTTY")
        val nonblock = fromWord (w_osval "O_NONBLOCK")
        val rsync    = fromWord (w_osval "O_RSYNC")
        val sync     = fromWord (w_osval "O_SYNC")
        val o_trunc  = w_osval "O_TRUNC"
        val trunc    = fromWord  o_trunc
        val o_creat  = w_osval "O_CREAT"
        val crflags  = o_wronly ++ o_creat ++ o_trunc

      end

    val openf' : string * s_word * s_word -> s_int = cfun "openf"
    fun openf (fname, omode, flags) =
          fd(openf'(fname, O.toWord flags ++ (omodeToWord omode), 0w0))
    fun createf (fname, omode, oflags, mode) = let
          val flags = O.o_creat ++ O.toWord oflags ++ (omodeToWord omode)
          in
            fd(openf'(fname, flags, S.toWord mode))
          end
    fun creat (fname, mode) =
          fd(openf'(fname, O.crflags, S.toWord mode))

    val umask' : s_word -> s_word = cfun "umask"
    fun umask mode = S.fromWord (umask' (S.toWord mode))

    val link' : string * string -> unit = cfun "link"
    fun link {old, new} = link'(old,new)
    val rename' : string * string -> unit = cfun "rename"
    fun rename {old, new} = rename'(old,new)
    val symlink' : string * string -> unit = cfun "symlink"
    fun symlink {old, new} = symlink'(old,new)

    val mkdir' : string * s_word -> unit = cfun "mkdir"
    fun mkdir (dirname, mode) = mkdir'(dirname, S.toWord mode)

    val mkfifo' : string * s_word -> unit = cfun "mkfifo"
    fun mkfifo (name, mode) = mkfifo'(name, S.toWord mode)

    val unlink : string -> unit = cfun "unlink"
    val rmdir : string -> unit = cfun "rmdir"
    val readlink : string -> string = cfun "readlink"

    val ftruncate' : s_int * Position.int -> unit = cfun "ftruncate"
    fun ftruncate (FD{fd,...}, len) = ftruncate' (fd, len);

    datatype dev = DEV of s_word
    fun devToWord (DEV i) = i
    fun wordToDev i = DEV i

(* FIXME: should support 64-bit i-nodes; this will require changing SysWord to be Word64
 * (or the Basis Library API should change).
    datatype ino = INO of Word64.word
    fun inoToWord (INO i) = SysWord.fromLarge(Word64.toLarge i)
    fun wordToIno i = INO(Word64.fromLarge(SysWord.toLarge i))
*)
    datatype ino = INO of s_word
    fun inoToWord (INO i) = i
    fun wordToIno i = INO i

    structure ST =
      struct
        datatype stat = ST of {
                 ftype : s_int,
                 mode  : S.mode,
                 ino   : ino,
                 dev   : dev,
                 nlink : int,
                 uid   : uid,
                 gid   : gid,
                 size  : Position.int,
                 atime : Time.time,
                 mtime : Time.time,
                 ctime : Time.time
               }
      (* The following assumes the C stat functions pull the
       * file type from the mode field and return the
       * integer below corresponding to the file type.
       *)
	fun isDir  (ST{ftype, ...}) = (ftype = 0x4000)
	fun isChr  (ST{ftype, ...}) = (ftype = 0x2000)
	fun isBlk  (ST{ftype, ...}) = (ftype = 0x6000)
	fun isReg  (ST{ftype, ...}) = (ftype = 0x8000)
	fun isFIFO (ST{ftype, ...}) = (ftype = 0x1000)
	fun isLink (ST{ftype, ...}) = (ftype = 0xA000)
	fun isSock (ST{ftype, ...}) = (ftype = 0xC000)

        fun mode (ST{mode,...}) = mode
        fun ino (ST{ino,...}) = ino
        fun dev (ST{dev,...}) = dev
        fun nlink (ST{nlink,...}) = nlink
        fun uid (ST{uid,...}) = uid
        fun gid (ST{gid,...}) = gid
        fun size (ST{size,...}) = size
        fun atime (ST{atime,...}) = atime
        fun mtime (ST{mtime,...}) = mtime
        fun ctime (ST{ctime,...}) = ctime
      end (* structure ST *)

  (* this layout needs to track c-libs/posix-filesys/stat.c *)
    type statrep =
      ( s_int			(* 1: file type *)
      * s_word			(* 2: mode *)
      * s_word			(* 3: ino *)	(* FIXME: should be Word64.word *)
      * s_word			(* 4: devno *)
      * s_word			(* 5: nlink *)
      * s_word			(* 6: uid *)
      * s_word			(* 7: gid *)
      * Position.int		(* 8: size *)
      * Word64.word		(* 9: atim (nanoseconds) *)
      * Word64.word		(* 10: mtim (nanoseconds) *)
      * Word64.word		(* 11: ctim (nanoseconds) *)
      )
    fun mkStat (sr : statrep) = ST.ST{
	    ftype = #1 sr,
            mode = S.fromWord (#2 sr),
            ino = INO (#3 sr),
            dev = DEV (#4 sr),
            nlink = SysWord.toInt(#5 sr),	(* probably should be an int in
						 * the run-time too.
						 *)
            uid = UID(#6 sr),
            gid = GID(#7 sr),
            size = #8 sr,
            atime = Time.fromNanoseconds (Word64.toLargeInt (#9 sr)),
            mtime = Time.fromNanoseconds (Word64.toLargeInt (#10 sr)),
            ctime = Time.fromNanoseconds (Word64.toLargeInt (#11 sr))
          }

    val stat' : string -> statrep = cfun "stat"
    val lstat' : string -> statrep = cfun "lstat"
    val fstat' : s_int -> statrep = cfun "fstat"
    fun stat fname = mkStat (stat' fname)
    fun lstat fname = mkStat (lstat' fname) (* POSIX 1003.1a *)
    fun fstat (FD{fd}) = mkStat (fstat' fd)

    datatype access_mode = A_READ | A_WRITE | A_EXEC
    val a_read = w_osval "A_READ"	(* R_OK *)
    val a_write = w_osval "A_WRITE"	(* W_OK *)
    val a_exec = w_osval "A_EXEC"	(* X_OK *)
    val a_file = w_osval "A_FILE"	(* F_OK *)
    fun amodeToWord [] = a_file
      | amodeToWord l = let
          fun amtoi (A_READ,v) = a_read ++ v
            | amtoi (A_WRITE,v) = a_write ++ v
            | amtoi (A_EXEC,v) = a_exec ++ v
          in
            List.foldl amtoi a_file l
          end
    val access' : string * s_word -> bool = cfun "access"
    fun access (fname, aml) = access'(fname, amodeToWord aml)

    val chmod' : string * s_word -> unit = cfun "chmod"
    fun chmod (fname, m) = chmod'(fname, S.toWord m)

    val fchmod' : s_int * s_word -> unit = cfun "fchmod"
    fun fchmod (FD{fd}, m) = fchmod'(fd, S.toWord m)

    val chown' : string * s_word * s_word -> unit = cfun "chown"
    fun chown (fname, UID uid, GID gid) = chown'(fname, uid, gid)

    val fchown' : s_int * s_word * s_word -> unit = cfun "fchown"
    fun fchown (fd, UID uid, GID gid) = fchown'(intOf fd, uid, gid)

    val utime' : string * Word64.word * Word64.word -> unit = cfun "utime"
    fun utime (file, NONE) = utime' (file, Word64Imp.~ 0w1, 0w0)
      | utime (file, SOME{actime, modtime}) = let
          val atime = Word64.fromLargeInt(Time.toNanoseconds actime)
          val mtime = Word64.fromLargeInt(Time.toNanoseconds modtime)
          in
            utime'(file, atime, mtime)
          end

    val pathconf  : (string * string) -> s_word option = cfun "pathconf"
    val fpathconf' : (s_int * string) -> s_word option = cfun "fpathconf"
    fun fpathconf (FD{fd}, s) = fpathconf'(fd, s)

  end (* structure POSIX_FileSys *)

end (* local *)
