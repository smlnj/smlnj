(* Copyright 1989 by AT&T Bell Laboratories *)
(*
RealConst: generate ML real constants.
RealConst uses long multiplication to find the correct bit pattern for
the real.  This method is slow, but accurate, and works to any precision,
which means that floats can be cross-compiled correctly.

The function emitreal should take (int * bool array * int) which represents
a real value as (sign * fraction * exponent).
The sign is 0 if the real is positive, 1 if negative.
The fraction is a boolean array representing the bits; note that the most
significant bit is in position 0.
The exponent is the binary exponent of the normalized fraction.
"Normalized" here means a number between 0 and 1.

The algorithm works inefficient on forms like 10000000.0; forms like 1E7 (with
no bogus zeros) are better.  Also inefficient on forms like 0E23 or 1E~212.
*)

signature PRIMREAL = sig
val significant : int
val minexp : int
val maxexp : int
val transreal : (int * (int*int->int) * int) -> string
end

signature REALCONST = sig
exception BadReal of string
val realconst : string -> string
end

functor RealConst(P : PRIMREAL) : REALCONST =
struct

open P

exception BadReal of string

(* Use more than the required precision, then round at the end.
   This criterion works well enough for the 53 bits required by
   Vax G format and IEEE double format, but has not been tested with other
   values of significant. *)

fun log2 0 = 1 | log2 i = 1+log2(i div 2)
val precision = significant + log2(maxexp-minexp) + 3

(* A float is a WHOLE "fraction" and an exponent base TWO. *)
type float = {frac : IntInf.int, exp : int}

val bigint = IntInf.fromInt
val plus = IntInf.+
val times = IntInf.*
(* size of a bigint in bits;
 *   bigsize 0 = 1
 *   bigsize i = floor(log2(i))+1
 * this means that:
 *   bigsize i = floor(log2(2*i+1))  *)
val bigone = IntInf.fromInt 1
fun bigsize x = IntInf.log2 (plus (plus(x, x), bigone))
fun getbit (b, w) =
    IntInf.compare (IntInf.andb (IntInf.~>> (b, w), bigone), bigone) = EQUAL
infix plus times

(* Take a bigint and return a function that will represent the
   fraction.  The function is called with two integers (start,width), and returns
   an integer represented by the bits from start to start+width-1.
   The high (1/2) bit is in position 0.  Assumes that
   the bigint is positive.  This will work if the bigint is smaller than
   the array or vice versa;  however, the number will be truncated, not
   rounded. *)
exception Bits
fun makebits frac (start,width) =
    let val s = bigsize frac
	fun onebit b = getbit(frac,Word.fromInt(s-1-b))
	fun b true = 1
          | b false = 0
	fun f 0 = b (onebit start)
          | f i = b (onebit(start+i)) + 2 * f(i-1)
    in if start < 0 orelse width < 0
	      then raise Bits
	      else f (width-1)
   end

(* round a float to n significant digits *)
local val one = bigint 1 in
fun round (float as {frac=f,exp=e},n) =
    let val shift = bigsize f + 1 - n
    in
	if shift <= 0 then float
	else {frac = if getbit(f, Word.fromInt (shift-1))
		     then IntInf.~>>(f, Word.fromInt shift) plus one
		     else IntInf.~>>(f, Word.fromInt shift),
	      exp = e + shift}
    end
end

(* maketenth:  create the float of one tenth, to any number of significant
   digits, with no rounding on the last digit. *)
local val zero = bigint 0 and one = bigint 1 and two = bigint 2 in
fun maketenth 1 = {frac=one,exp= ~4}
  | maketenth n =
    let val {frac,exp} = maketenth(n-1)
	val rec tenthbit = fn 0 => zero | 1 => one
			    | 2 => one | 3 => zero | n => tenthbit(n mod 4)
	val f = (frac times two) plus tenthbit n
	val e = exp - 1
    in
	{frac=f,exp=e}
    end
end

(* float values ten and one tenth, to the correct precision. *)
val ten = {frac=bigint 5, exp = 1}
val tenth = round(maketenth(precision+1),precision)

(* Multiplies two floats together to the correct precision *)
fun mult {frac=f1,exp=e1} {frac=f2,exp=e2} =
    let val f = f1 times f2
	val e : int = e1 + e2
	    (* shouldn't need the type constraint, our comp bug *)
    in
	round({frac=f,exp=e},precision)
    end

(* Create a dynamic array of powers of ten *)
structure DFA = DynamicArrayFn(
  struct open Array
    type float = {frac : IntInf.int, exp : int}
    type elem = unit->float
    type array = elem array
    type vector = elem vector
  end)

local open Array List DFA
      infix 9 sub
      exception Unknown
      fun makelem e = (fn () => e)
      val one = {frac=bigint 1,exp=0}
in
    val pos10 = array(0, fn () => raise Unknown)	(* 10^2^n *)
    val _ = update(pos10,0,makelem ten)
    val neg10 = array(0, fn () => raise Unknown)	(* 10^~2^n *)
    val _ = update(neg10,0,makelem tenth)
    fun access(arr,n) = (arr sub n) ()
			handle Unknown => let val last = access(arr,n-1)
					       val new = mult last last
					   in  update(arr,n,makelem new);
					       new
					   end

    fun pow10_2 0 = one
      | pow10_2 n = if n > 0 then access(pos10,n-1) else access(neg10,~n-1)
    fun raisepower(f,0) = f
      | raisepower(f,e) =
	    let val sign = if e<0 then ~1 else 1
		fun power(f,p) = mult f (pow10_2(sign*p))
		fun raisep(f,0w0,_) = f
		  | raisep(f,e,p) =
		    if Word.andb(e, 0w1) = 0w1 then 
		      raisep(power(f,p), Word.>>(e, 0w1), p+1)
		    else raisep(f, Word.>>(e, 0w1), p+1)
	    in  raisep(f, Word.fromInt (abs e), 1)
	    end
end

(* Takes a string list of the form {digit*.[digit*]}, and returns a bigint and
   the exponent base 10.  Requires that the list contain a decimal point and
   no trailing zeros (useless zeros after the decimal point). *)
local val ten = bigint 10 and zero = bigint 0 in
fun reducefrac f =
    let fun getexp nil = 0
	  | getexp (#"." :: _) = 0
	  | getexp (_ :: tl) = getexp tl - 1
	fun getwhole nil = zero
	  | getwhole (#"." :: tl) = getwhole tl
	  | getwhole (#"0" :: tl) = ten times getwhole tl
	  | getwhole (n::tl) =
	      bigint(Char.ord n - Char.ord #"0") plus (ten times getwhole tl)
	val backwards = rev f
	val whole = getwhole backwards
	val exp = getexp backwards
    in
	(whole,exp)
    end
end

(* Takes a legal ML float string and returns an (int * bigint * int)
   which is the sign, whole "fraction", and power of ten exponent *)
fun getparts s =
    let datatype trailing = SIGNIFICANT | TRAILING
	(* separate the fraction from the exponent, adding a decimal point if
	   there is none and eliminating trailing zeros *)
	fun separate (nil,s) = (nil,nil,s)
	  | separate ((#"E" | #"e")::tl,SIGNIFICANT) = ([#"."],tl,SIGNIFICANT)
	  | separate ((#"E" | #"e")::tl,TRAILING) = (nil,tl,TRAILING)
	  | separate (#"0"::tl,s) =
		let val (r,e,s) = separate(tl,s)
		in  case s of TRAILING => (r,e,TRAILING)
			    | SIGNIFICANT => (#"0"::r,e,SIGNIFICANT)
		end
	  | separate (#"."::tl,_) =
		let val (r,e,_) = separate(tl,TRAILING)
		in  (#"."::r,e,SIGNIFICANT)
		end
	  | separate (hd::tl,s) =
		let val (r,e,_) = separate(tl,s)
		in  (hd::r,e,SIGNIFICANT)
		end
	val (unsigned,sign) = (case explode s
	       of (#"~"::more) => (more,1)
		| other => (other,0)
	      (* end case *))
	val (frac_s,exp_s,_) = separate(unsigned,SIGNIFICANT)
	fun atoi strlist =
	    let	val numlist = map (fn n => Char.ord n - Char.ord #"0") strlist
	    in  List.foldl (fn (a:int,b) => b*10 + a) 0 numlist
	    end
	val exp10 = (case exp_s of nil => 0
				 | #"~"::more => ~(atoi more)
				 | other => atoi other)
		    handle Overflow => raise BadReal s
	val (frac,exp) = reducefrac frac_s
    in 
	(sign,frac,exp10 + exp)
    end

fun realconst f = 
    let val (sign,frac10,exp10) = getparts f
	val float = raisepower(round({frac=frac10,exp=0},precision),exp10)
	val (newf as {frac,exp}) = round(float,significant+1)
	val size = bigsize frac
	val bits = makebits frac
	val exp = exp+size
    in transreal(
	case size 
	 of 0 => (sign,bits,0)
	  | _ => if exp<minexp orelse exp>maxexp then raise BadReal f
		  else (sign,bits,exp))
    end

end (* functor RealConst *)

