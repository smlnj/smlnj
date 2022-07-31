(* date.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * The SML Basis Library Date module.  This code is partially based on
 * ideas from the paper
 *
 *	Calendrical Calculations
 *	by Nachum Dershowitz and Edward M. Reingold
 *	Software---Practice & Experience,
 *	vol. 20, no. 9 (September, 1990), pp. 899--928.
 *
 * C++, Lisp, and EmacsLisp code from that paper can be found at
 *
 *	http://emr.cs.iit.edu/~reingold/calendars.shtml
 *
 * The SML/NJ runtime system interface uses unsigned 64-bit values (in nanoseconds
 * since the Epoch) to represent time values.
 *)

structure Date : DATE =
  struct

    structure Int = IntImp
    structure Int32 = Int32Imp
    structure Word64 = Word64Imp
    structure IntInf = IntInfImp
    structure String = StringImp
    structure Time = TimeImp
    structure SS = InitSubstring

    exception Date

  (* the tuple type used to communicate with C; this 9-tuple has the
   * fields:
   *   tm_sec, tm_min, tm_hour, tm_mday, tm_mon, tm_year,
   *   tm_wday, tm_yday,
   *   tm_isdst.
   *)
    type tm = (int * int * int * int * int * int * int * int * int)

    fun tm_sec   ((s, _, _, _, _, _, _, _, _) : tm) = s
    fun tm_min   ((_, m, _, _, _, _, _, _, _) : tm) = m
    fun tm_hour  ((_, _, h, _, _, _, _, _, _) : tm) = h
    fun tm_mday  ((_, _, _, d, _, _, _, _, _) : tm) = d
    fun tm_mon   ((_, _, _, _, m, _, _, _, _) : tm) = m
    fun tm_year  ((_, _, _, _, _, y, _, _, _) : tm) = y
    fun tm_wday  ((_, _, _, _, _, _, d, _, _) : tm) = d
    fun tm_yday  ((_, _, _, _, _, _, _, d, _) : tm) = d
    fun tm_isdst ((_, _, _, _, _, _, _, _, i) : tm) = i
    fun set_tm_isdst ((a, b, c, d, e, f, g, h, i) : tm, i') : tm =
	  (a, b, c, d, e, f, g, h, i')

  (* wrap a C function call with a handler that maps SysErr
   * exception into Date exceptions.
   *)
    fun wrap f x = (f x) handle _ => raise Date

  (* note: mkTime assumes the tm structure passed to it reflects
   * the local time zone
   *)
    val localTime' : Word64.word -> tm
	  = wrap (CInterface.c_function "SMLNJ-Date" "localTime")
    val gmTime' : Word64.word -> tm
	  = wrap (CInterface.c_function "SMLNJ-Date" "gmTime")
    val mkTime' : tm -> Word64.word
	  = wrap (CInterface.c_function "SMLNJ-Date" "mkTime")
    val strfTime : (string * tm) -> string
	  = wrap (CInterface.c_function "SMLNJ-Date" "strfTime")

  (* conversions between integer numbers of seconds (used by runtime) and Time.time values *)
    fun nsToTime s = Time.fromNanoseconds (Word64.toLargeInt s)
    fun timeToNs t = Word64.fromLargeInt (Time.toNanoseconds t)

    val localTime = localTime' o timeToNs
    val gmTime = gmTime' o timeToNs

  (* a function to return the offset from UTC of the time t in the local timezone.
   * This value reflects not only the geographical location of the host system, but
   * also daylight savings time (if it is in effect).  Note that this value is
   * positive to the east of UTC and negative to the west.  Add it to UTC to get
   * the local time.
   * Note that the offset is signed!!!
   *)
    local
      val toTime = Time.fromSeconds o Int32.toLarge
      val localOffsetForTime' : Word64.word -> Int32.int =
	    wrap (CInterface.c_function "SMLNJ-Date" "localOffsetForTime")
      val localOffset' : unit -> Int32.int =
	    wrap (CInterface.c_function "SMLNJ-Date" "localOffset")
    in
    val localOffsetForTime = toTime o localOffsetForTime'
  (* localOffset for the current time *)
    val localOffset = toTime o localOffset'
    end (* local *)

    datatype weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun

    datatype month
      = Jan | Feb | Mar | Apr | May | Jun
      | Jul | Aug | Sep | Oct | Nov | Dec

    datatype date = DATE of {
	year : int,			(* 1.. *)
	month : month,
	day : int,			(* 1..31 *)
	hour : int,			(* 0..23 *)
	minute : int,			(* 0..59 *)
	second : int,			(* 0..61 (allowing for leap seconds) *)
	offset : Time.time option,	(* signed delta east of UTC; add to UTC to get local time *)
	wday : weekday,
	yday : int,			(* 0..365 *)
	isDst : bool option
      }

  (* tables for mapping integers to days/months *)
    val dayTbl = #[Sun, Mon, Tue, Wed, Thu, Fri, Sat]
    val monthTbl = #[Jan, Feb, Mar, Apr, May, Jun, Jul,
		     Aug, Sep, Oct, Nov, Dec]

    fun dayToInt Sun = 0
      | dayToInt Mon = 1
      | dayToInt Tue = 2
      | dayToInt Wed = 3
      | dayToInt Thu = 4
      | dayToInt Fri = 5
      | dayToInt Sat = 6

  (* careful about this: the month numbers are 0-11 *)
    fun monthToInt Jan = 0
      | monthToInt Feb = 1
      | monthToInt Mar = 2
      | monthToInt Apr = 3
      | monthToInt May = 4
      | monthToInt Jun = 5
      | monthToInt Jul = 6
      | monthToInt Aug = 7
      | monthToInt Sep = 8
      | monthToInt Oct = 9
      | monthToInt Nov = 10
      | monthToInt Dec = 11

    fun year (DATE{year, ...}) = year
    fun month (DATE{month, ...}) = month
    fun day (DATE{day, ...}) = day
    fun hour (DATE{hour, ...}) = hour
    fun minute (DATE{minute, ...}) = minute
    fun second (DATE{second, ...}) = second
    fun weekDay (DATE{wday, ...}) = wday
    fun yearDay (DATE{yday, ...}) = yday
    fun isDst (DATE{isDst, ...}) = isDst
    fun offset (DATE{offset,...}) = offset

  (* convert runtime tm tuple to date type *)
    fun tm2date (tm : tm, offset) = DATE{
	    year = tm_year tm,
	    month = Vector.sub(monthTbl, tm_mon tm),
	    day = tm_mday tm,
	    hour = tm_hour tm,
	    minute = tm_min tm,
	    second = tm_sec tm,
	    offset = offset,
	    wday = Vector.sub(dayTbl, tm_wday tm),
	    yday = tm_yday tm,
	    isDst = if (tm_isdst tm < 0) then NONE else SOME(tm_isdst tm > 0)
	  }

  (* convert date type to runtime tm tuple *)
    fun date2tm (DATE d) : tm = (
	    #second d,			(* tm_sec *)
	    #minute d,			(* tm_min *)
	    #hour d,			(* tm_hour *)
	    #day d,			(* tm_mday *)
	    monthToInt(#month d),	(* tm_mon *)
	    #year d,			(* tm_year *)
	    dayToInt(#wday d),		(* tm_wday *)
	    #yday d,			(* tm_yday *)
	    case (#isDst d)		(* tm_isdst *)
	     of NONE => ~1
	      | (SOME false) => 0
	      | (SOME true) => 1
	    (* end case *)
	  )

    type gregorian_date = {
	year : int,	(* 1.. (* year 1 == 1 AD *) *)
	month : int,	(* 1..12 *)
	day : int	(* 1..31 *)
      }

    local
      val monthLengths = #[31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    (* the number of days in the year before the 1st of the month; it is
     * the prefix sum of monthLengths
     *)
      val daysBeforeMonth = #[0,31,59,90,120,151,181,212,243,273,304,334];
    (* for greforian dates, months are numbered from 1 *)
      val febId = 2
    in
  (* return true for leap years *)
    fun isLeapYear y = (y mod 4 = 0) andalso ((y mod 100 <> 0) orelse (y mod 400 = 0))
  (* return the number of days in a month given the year and month *)
    fun lastDayOfGregorianMonth {month, year} = if month = febId andalso isLeapYear year
	  then 29
	  else Vector.sub(monthLengths, month-1)
  (* return the # of days of the year before the given Gregorian date; 0 is January 1st *)
    fun daysInYear {year, month, day} = let
	  val n = Vector.sub(daysBeforeMonth, month-1) + day
	  in
	    if (febId < month) andalso isLeapYear year
	      then n
	      else n-1
	  end
  (* convert a Gregorian date to an absolute day (i.e., the day index starting with day 1
   * being January 1, year 1 AD.
   *)
    fun toAbsoluteDay {year, month, day} = let
	  val priorYears = year - 1
	  val n = daysInYear {year=year, month=month, day=day}
	  val n = n + 365 * priorYears		(* days in previous years ignoring leap days *)
	  val n = n + priorYears div 4		(* Julian leap days before this year... *)
	  val n = n - priorYears div 100	(* ...minus prior century years... *)
	  val n = n + priorYears div 400	(* ...plus prior years divisible by 400 *)
	  in
	    n + 1
	  end
  (* convert an abosolute day to a Gregorian date *)
    fun fromAbsoluteDay d = let
	(* search forward from a lower bound on the year *)
	  val year = let
		fun lp y = if toAbsoluteDay{year=y+1, month=1, day=1} <= d
		      then lp (y+1)
		      else y
		in
		  lp (d div 366)
		end
	(* search forward from January *)
	  val month = let
		fun lp m = let
		      val d' = lastDayOfGregorianMonth{month = m, year = year}
		      in
			if toAbsoluteDay{year=year, month=m, day=d'} < d
			  then lp(m+1)
			  else m
		      end
		in
		  lp 1
		end
	  in {
	    year = year, month = month,
	    day = d - toAbsoluteDay{year=year, month=month, day=1} + 1
	  } end
  (* return the number of days in a given year *)
    fun yearLength year = if isLeapYear year then 366 else 365
    end
(* DEBUG **
  (* some internal test cases *)
    local
    fun test name true = print(concat["!!! test ", name, " OK\n"])
      | test name false = print(concat["!!! test ", name, " FAILED\n"])
    in
    val _ = test "t01" (toAbsoluteDay {year=1, month=1, day=1} = 1)
    val _ = test "t02" (toAbsoluteDay {year=1, month=12, day=31} = 365)
    val _ = test "t03" (toAbsoluteDay {year=2, month=1, day=1} = 366)
    val _ = test "t04" (daysInYear {year=1, month=12, day=31} = 364)
    val _ = test "t05" (daysInYear {year=2000, month=12, day=31} = 365)
    val _ = test "t06" (isLeapYear 2000 andalso isLeapYear 2004 andalso isLeapYear 2400)
    val _ = test "t07" (not (List.exists isLeapYear [1800, 1900, 2100, 2200, 2300, 2500]))
    val _ = test "t08" (toAbsoluteDay(fromAbsoluteDay 1) = 1)
    val _ = test "t09" (toAbsoluteDay(fromAbsoluteDay 364) = 364)
    val _ = test "t10" (toAbsoluteDay(fromAbsoluteDay 365) = 365)
    val _ = test "t11" (toAbsoluteDay(fromAbsoluteDay 366) = 366)
    val _ = test "t12" (toAbsoluteDay(fromAbsoluteDay 1460) = 1460)
    val _ = test "t13" (toAbsoluteDay(fromAbsoluteDay 1461) = 1461)
    val _ = test "t14" (toAbsoluteDay(fromAbsoluteDay 5000) = 5000)
    end
** DEBUG *)

  (* canonicalization of a date record.  Note that the month values are
   * 0 based (unlike the 1-based values of a gregorian_date).
   *)
    fun normalizeDate {year, month, day, hour, minute, second} = let
	  fun divMod (a, b) = let
		val q = a div b
		in
		  (q, a - q * b)
		end
	  val (minute, second) = let
		val (m, s) = divMod (second, 60)
		in
		  (minute + m, s)
		end
	  val (hour, minute) = let
		val (h, m) = divMod (minute, 60)
		in
		  (hour + h, m)
		end
	  val (absDay, hour) = let
		val (d, h) = divMod (hour, 24)
		val day = day + d
		in
		(* note that toAbsoluteDay will handle the case where day <= 0 *)
		  (toAbsoluteDay{year = year, month=month+1, day=day}, h)
		end
	(* we only allow CE (aka AD) dates *)
	  val _ = if (absDay <= 0) then raise Date else ()
	  val (year, month, day) = let
		val {year, month, day} = fromAbsoluteDay absDay
		in
		  (year, month-1, day)
		end
	  in {
	    year = year, month = month, day = day, absDay = absDay,
	    hour = hour, minute = minute, second = second
	  } end

    fun date {year, month, day, hour, minute, second, offset} = let
	  val (secOffset, offset) = (case offset
		 of NONE => (0, NONE)
		  | SOME t => let
		    (* normalize offset to range of ~86399..86399 (24 hours in seconds) *)
		      val secs = IntInf.rem(Time.toSeconds t, 86400)
		      in
			(Int.fromLarge secs, SOME(Time.fromSeconds secs))
		      end
		(* end case *))
	  val normDate = normalizeDate {
		  year = year, month = monthToInt month, day = day,
		  hour = hour, minute = minute, second = second + secOffset
		}
	(* check that we are in AD at least *)
	  val _ = if #year normDate < 0 then raise Date else ()
	  in
	    DATE{
		year = #year normDate,
		month = Vector.sub (monthTbl, #month normDate),
		day = #day normDate,
		hour = #hour normDate,
		minute = #minute normDate,
		second = #second normDate,
		offset = offset,
		isDst = NONE,
		yday = daysInYear {
		    year = #year normDate, month = #month normDate + 1, day = #day normDate
		  },
		wday = Vector.sub(dayTbl, #absDay normDate mod 7)
	      }
	  end

    fun fromTimeLocal t = let
	  val offset = localOffsetForTime (timeToNs t)
	  in
	    tm2date (localTime t, SOME offset)
	  end

    fun fromTimeUniv t = tm2date (gmTime t, SOME Time.zeroTime)

  (* return the UTC time corresponding to the given date. *)
    fun toTime (date as DATE{offset, ...}) = let
	  val t = mkTime' (date2tm date)
	  in
	    case offset
	     of NONE => nsToTime t
	      | SOME offset =>
		(* note that representation of a date is canonical, which means that the
		 * offset has already been applied, so we do not need to adjust by the
		 * date's offset.  On the other hand, mkTime' returns the _local_ time,
		 * so we do need to adjust for the local offset.
		 *)
		  Time.+(nsToTime t, localOffsetForTime t)  (* converts local time to UTC *)
	    (* end case *)
	  end

  (* date comparison does not take into account the offset
   * thus, it does not compare dates in different time zones
   *)
    fun compare (DATE d1, DATE d2) = let
	  fun cmp (sel, k) = (case Int.compare (sel d1, sel d2)
		 of EQUAL => k()
		  | order => order
		(* end case *))
	  in
	    cmp (#year, fn () =>
	    cmp (monthToInt o #month, fn () =>
	    cmp (#day, fn () =>
	    cmp (#hour, fn () =>
	    cmp (#minute, fn () =>
	    cmp (#second, fn () => EQUAL))))))
	  end


  (***** String conversions *****)

  (* the size of the runtime system character buffer, not including space for the '\0' *)
    val fmtBuf = 512-1
    fun fmt fmtStr = let
	(* get a format character; the next character in start should be #"%" (or else
	 * start is empty.  Returns a triple (maxLen, frag, rest), where maxLen is an
	 * upperbound on the expansion of the format string, frag is the format string
	 * and rest is the rest of the substring.
	 *)
	  fun getFmtC start = (case SS.getc start
		 of SOME(_, rest) => let
		      fun continue (len, ss') = (len, SS.slice(start, 0, SOME 2), ss')
		      in
			case SS.getc rest
			 of NONE => (1, SS.full "%", rest)
			  | SOME(#"a", ss') => continue(3, ss')
			  | SOME(#"A", ss') => continue(20, ss')
			  | SOME(#"b", ss') => continue(3, ss')
			  | SOME(#"B", ss') => continue(20, ss')
			  | SOME(#"c", ss') => continue(24, ss')
			  | SOME(#"d", ss') => continue(2, ss')
			  | SOME(#"H", ss') => continue(2, ss')
			  | SOME(#"I", ss') => continue(2, ss')
			  | SOME(#"j", ss') => continue(3, ss')
			  | SOME(#"m", ss') => continue(2, ss')
			  | SOME(#"M", ss') => continue(2, ss')
			  | SOME(#"p", ss') => continue(3, ss')
			  | SOME(#"S", ss') => continue(2, ss')
			  | SOME(#"U", ss') => continue(2, ss')
			  | SOME(#"w", ss') => continue(1, ss')
			  | SOME(#"W", ss') => continue(2, ss')
			  | SOME(#"x", ss') => continue(24, ss')
			  | SOME(#"X", ss') => continue(24, ss')
			  | SOME(#"y", ss') => continue(2, ss')
			  | SOME(#"Y", ss') => continue(4, ss')
			  | SOME(#"Z", ss') => continue(3, ss')
			  | SOME(c, ss') => (1, SS.full(String.str c), ss')
			(* end case *)
		      end
		  | NONE => (0, start, start)
		(* end case *))
	  fun mkFmtFn (frags, fmtFns) = if List.null frags
		then fmtFns
		else let
		  val s = SS.concat(List.rev frags)
		  in
		    (fn tm => strfTime(s, tm)) :: fmtFns
		  end
	  fun notPct #"%" = false | notPct _ = true
	  fun scan (ss, totLen, frags, fmtFns) = let
		val (ss1, ss2) = SS.splitl notPct ss
		val n = SS.size ss1
		val (totLen, frags, fmtFns) = if (n = 0)
			then (totLen, frags, fmtFns)
		      else if (totLen+n >= fmtBuf)
			then let
			  val fmtFns = mkFmtFn(frags, fmtFns)
			  val s = SS.string ss1
			  in
			    (0, [], (fn _ => s) :: fmtFns)
			  end
			else (totLen+n, ss1::frags, fmtFns)
		in
		  case getFmtC ss2
		   of (0, _, _) => List.rev(mkFmtFn (frags, fmtFns))
		    | (n, frag, rest) => if (totLen + n >= fmtBuf)
			then let
			  val fmtFns = mkFmtFn(frags, fmtFns)
			  in
			    scan (rest, n, [frag], fmtFns)
			  end
			else scan (rest, totLen+n, frag::frags, fmtFns)
		  (* end case *)
		end
	  val fmtFns = scan (SS.full fmtStr, 0, [], [])
	  in
	    fn d => let val tm = date2tm d in String.concat(List.map (fn f => f tm) fmtFns) end
	  end

    val toString = fmt "%a %b %d %H:%M:%S %Y"

  (* Date scanner *)
    fun scan getc s = let
	  fun getword s = StringCvt.splitl Char.isAlpha getc s
	(* consume the character c from the stream s and then pass the remaining
	 * stream to the continuation k.  Returns NONE if a different character
	 * is encountered.
	 *)
	  fun expect c s k = (case getc s
		 of NONE => NONE
		  | SOME(c', s') => if c = c' then k s' else NONE
		(* end case *))
	  fun getdig s = (case getc s
		of NONE => NONE
		 | SOME(c, s') => if Char.isDigit c
		    then SOME (Char.ord c - Char.ord #"0", s')
		    else NONE
  		(* end case *))
	  fun get2dig s = (case getdig s
		 of SOME(c1, s') => (case getdig s'
		       of SOME (c2, s'') => SOME (10 * c1 + c2, s'')
		        | NONE => NONE
		      (* end case *))
		  | NONE => NONE
    		(* end case *))
	(* day can be two digits or one digit preceded by a space *)
	  fun getday s = (case get2dig s
		 of NONE => expect #" " s (fn s' => getdig s')
		  | (res as SOME (n, s')) => res
  		(* end case *))
	  fun year0 (wday, mon, d, hr, mn, sc) s = (case Int.scan StringCvt.DEC getc s
		 of NONE => NONE
		  | SOME (yr, s') => (SOME(date {
			year = yr, month = mon, day = d,
			hour = hr, minute = mn, second = sc,
			offset = NONE
		      }, s')) handle _ => NONE
		(* end case *))
	  fun year args s = expect #" " s (year0 args)
	  fun second0 (wday, mon, d, hr, mn) s = (case get2dig s
		 of NONE => NONE
		  | SOME (sc, s') => year (wday, mon, d, hr, mn, sc) s'
		(* end case *))
	  fun second args s = expect #":" s (second0 args)
	  fun minute0 (wday, mon, d, hr) s = (case get2dig s
		 of NONE => NONE
		  | SOME (mn, s') => second (wday, mon, d, hr, mn) s'
		(* end case *))
	  fun minute args s = expect #":" s (minute0 args)
	  fun time0 (wday, mon, d) s = (case get2dig s
		 of NONE => NONE
		  | SOME (hr, s') => minute (wday, mon, d, hr) s'
		(* end case *))
	  fun time args s = expect #" " s (time0 args)
	  fun mday0 (wday, mon) s = (case getday s
		 of NONE => NONE
		  | SOME (d, s') => time (wday, mon, d) s'
  		(* end case *))
	  fun mday args s = expect #" " s (mday0 args)
	  fun month0 wday s = (case getword s
		 of ("Jan", s') => mday (wday, Jan) s'
		  | ("Feb", s') => mday (wday, Feb) s'
		  | ("Mar", s') => mday (wday, Mar) s'
		  | ("Apr", s') => mday (wday, Apr) s'
		  | ("May", s') => mday (wday, May) s'
		  | ("Jun", s') => mday (wday, Jun) s'
		  | ("Jul", s') => mday (wday, Jul) s'
		  | ("Aug", s') => mday (wday, Aug) s'
		  | ("Sep", s') => mday (wday, Sep) s'
		  | ("Oct", s') => mday (wday, Oct) s'
		  | ("Nov", s') => mday (wday, Nov) s'
		  | ("Dec", s') => mday (wday, Dec) s'
		  | _ => NONE
		(* end case *))
	  fun month wday s = expect #" " s (month0 wday)
	  fun wday s = (case getword s
		 of ("Sun", s') => month Sun s'
		  | ("Mon", s') => month Mon s'
		  | ("Tue", s') => month Tue s'
		  | ("Wed", s') => month Wed s'
		  | ("Thu", s') => month Thu s'
		  | ("Fri", s') => month Fri s'
		  | ("Sat", s') => month Sat s'
		  | _ => NONE
		(* end case *))
	  in
	    wday s
	  end (* scan *)

    fun fromString s = StringCvt.scanString scan s

  end (* Date *)
