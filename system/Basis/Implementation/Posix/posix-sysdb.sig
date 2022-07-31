(* posix-sysdb.sig
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Signature for POSIX 1003.1 system data-base operations
 *)

signature POSIX_SYS_DB =
  sig
    eqtype uid
    eqtype gid

    structure Passwd :
      sig
        type passwd

        val name  : passwd -> string
        val uid   : passwd -> uid
        val gid   : passwd -> gid
        val home  : passwd -> string
        val shell : passwd -> string

      end

    structure Group :
      sig
        type group

        val name    : group -> string
        val gid     : group -> gid
        val members : group -> string list

      end

    val getgrgid : gid -> Group.group
    val getgrnam : string -> Group.group
    val getpwuid : uid -> Passwd.passwd
    val getpwnam : string -> Passwd.passwd

  end (* signature POSIX_SYS_DB *)

