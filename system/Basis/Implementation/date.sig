(* date.sig
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

signature DATE =
  sig

    datatype weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun

    datatype month
      = Jan | Feb | Mar | Apr | May | Jun
      | Jul | Aug | Sep | Oct | Nov | Dec

    type date

    (* raised on errors, as described below *)
    exception Date

    (* returns the year (e.g., 1997) *)
    val year    : date -> int
    (* returns the month *)
    val month   : date -> month
    (* returns the day of the month *)
    val day     : date -> int
    (* returns the hour *)
    val hour    : date -> int
    (* returns the minute *)
    val minute  : date -> int
    (* returns the second *)
    val second  : date -> int
    (* returns the day of the week *)
    val weekDay : date -> weekday
    (* returns the day of the year *)
    val yearDay : date -> int
    (* returns SOME(true) if daylight savings time is in effect; returns
     * SOME(false) if not, and returns NONE if we don't know.
     *)
    val isDst   : date -> bool option
    (* return time west of UTC.  NONE is localtime, SOME(Time.zeroTime) is UTC. *)
    val offset  : date -> Time.time option
    (* offset from UTC for the local time zone *)
    val localOffset : unit -> Time.time

    (* creates a date from the given values. *)
    val date : {
            year   : int,
            month  : month,
            day    : int,
            hour   : int,
            minute : int,
            second : int,
            offset : Time.time option
          } -> date

    (* returns the date for the given time in the local timezone.
     * this is like the ANSI C function localtime.
     * was: fromTime
     *)
    val fromTimeLocal : Time.time -> date
    (* returns the date for the given time in the UTC timezone.
     * this is like the ANSI C function gmtime.
     * was: fromUTC
     *)
    val fromTimeUniv : Time.time -> date
    (* returns the UTC time value corresponding to the date.  This function
     * raises Date exception if the date cannot be represented as a time value.
     *)
    val toTime : date -> Time.time

    val toString : date -> string
    val fmt : string -> date -> string

    val fromString : string -> date option
    val scan : (char, 'a) StringCvt.reader -> (date, 'a) StringCvt.reader

    (* returns the relative order of two dates. *)
    val compare : (date * date) -> order

  end;
