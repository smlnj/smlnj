(* init-cleanup.sml
 *
 * COPYRIGHT (c) 2001 Bell Labs, Lucent Technologies
 *)

structure InitCleanup : sig end =
  struct

  (* Add the standard cleaners.  The order here is important: I/O needs to be
   * after Channels&Mailboxes, but before Servers (since server cleanup may
   * depend on I/O).
   *)
    val _ = (
	  CleanUp.addCleaner CleanUp.chanCleaner;
	  CleanUp.addCleaner CleanIO.ioCleaner;
	  CleanUp.addCleaner CleanUp.servCleaner)

  end
