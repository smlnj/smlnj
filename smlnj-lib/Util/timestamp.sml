(* timestamp.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * Get timestamps that respect the `SOURCE_DATE_EPOCH` environment variable
 * as described at https://reproducible-builds.org/specs/source-date-epoch.
 *)

structure Timestamp : sig

    (* this exception is raised when the `SOURCE_DATE_EPOCH` environment
     * variable is defined, but malformed.
     *)
    exception InvalidSourceDateEpoch of string

    (* make a timestamp from a time value.  This function normally behaves as
     * the identity, unless the variable `SOURCE_DATE_EPOCH` is set in the
     * environment, in which case it returns a time value that is the number
     * of seconds (excluding leap seconds) since January 1, 1970 00:00:00 UTC.
     * If `SOURCE_DATE_EPOCH` is defined, but malformed, then the exception
     * `InvalidSourceDateEpoch s` is raised, where `s` is the value of the
     * `SOURCE_DATE_EPOCH` environment variable.
     *)
    val makeTimestamp : Time.time -> Time.time

    (* get the current time as a timestamp *)
    val now : unit -> Time.time

  end = struct

    exception InvalidSourceDateEpoch of string

    fun getSDE () = OS.Process.getEnv "SOURCE_DATE_EPOCH"

    (* time "0" is January 1, 1970 UTC *)
    val base = Date.date {
            year = 1970, month = Date.Jan, day = 1,
            hour = 0, minute = 0, second = 0,
            offset = SOME Time.zeroTime
          }

    fun makeTimestamp origTime = (case getSDE()
           of NONE => origTime
            | SOME v => (case IntInf.fromString v
                 of SOME n => let
                      val t = Time.toSeconds(Date.toTime base) + n
                      in
                        if (t < 0)
                          then raise InvalidSourceDateEpoch v
                          else (Time.fromSeconds t
                            handle _ => raise InvalidSourceDateEpoch v)
                      end
                  | NONE => raise InvalidSourceDateEpoch v
                (* end case *))
          (* end case *))

    fun now () = makeTimestamp (Time.now())

  end
