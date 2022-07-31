(*
 * CM timestamp semantics.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure TStamp = struct

    datatype t =
	NOTSTAMP
      | TSTAMP of Time.time

    val ancient = TSTAMP (Time.zeroTime)

    (* We consider a target good if it has the same time stamp
     * as the source.  A target that isn't there is never good,
     * and if there is a target but no source, then we assume the
     * target to be ok. *)
    fun needsUpdate { target = NOTSTAMP, ... } = true
      | needsUpdate { source = NOTSTAMP, ... } = false
      | needsUpdate { source = TSTAMP st, target = TSTAMP tt } =
	Time.compare (st, tt) <> EQUAL

    fun fmodTime f = TSTAMP (OS.FileSys.modTime f) handle _ => NOTSTAMP
    fun setTime (f, NOTSTAMP) = ()
      | setTime (f, TSTAMP t) = OS.FileSys.setTime (f, SOME t)

    fun max (TSTAMP t, TSTAMP t') = TSTAMP (if Time.< (t, t') then t' else t)
      | max _ = NOTSTAMP

    fun toString NOTSTAMP = "none"
      | toString (TSTAMP t) = Time.toString t
end
