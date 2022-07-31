(* format-comb-sig.sml
 *
 * COPYRIGHT (c) 2007 The Fellowship of SML/NJ
 *
 *   Well-typed "printf" for SML, aka "Unparsing Combinators".
 *     This code was written by Matthias Blume (2002).  Inspiration
 *     obtained from Olivier Danvy's "Functional Unparsing" work.
 *
 * Description:
 *
 * The idea is to use combinators for constructing something akin to
 * the format string of C's printf function.  The difference is, however,
 * that our formats aren't strings.  Instead, format( fragment)s have
 * meaningful types, and passing them to function "format" results
 * in a curried function whose arguments have precisely the types that
 * correspond to argument-consuming parts of the format.  (Such
 * argument-consuming parts are similar to the %-specifications of printf.)
 *
 * Here is how the typing works: There is an underlying notion of
 * "abstract formats" of type 'a format.  However, the user operates
 * at the level of "format fragments" which have type ('a, 'b)
 * fragment and are typically polymorphic in 'a (where 'b is
 * instantiated to some type containing 'a).  Fragments are
 * functions from formats to formats and can be composed freely using
 * the function composition operator 'o'.  This form of format
 * composition translates to a corresponding concatenation of the
 * resulting output.
 *
 * Fragments are composed from two kids of primitve fragments called
 * "elements" and "glue", respectively.  An "element" is a fragment that
 * consumes some argument (which thanks to the typing magic appears as a
 * curried argument when the format gets executed).  As "glue" we refer
 * to fragments that do not consume arguments but merely insert fixed
 * text (fixed at format construction time) into the output.
 *
 * There are also adjustment operations that pad, trim, or fit the output
 * of entire fragments (primitive or not) to a given size.
 *
 * A number of elements and some glue has been predefined.  Here are
 * examples on how to use this facility:
 *
 *  open FormatComb
 *
 *  format nothing                      ==> ""
 *
 *  format int                          ==> fn: int -> string
 *  format int 1234                     ==> "1234"
 *
 *  format (text "The square of " o int o text " is " o int o text ".")
 *                                      ==> fn: int -> int -> string
 *  format (text "The square of " o int o text " is " o int o text ".") 2 4
 *                                      ==> "The square of 2 is 4."
 *
 *  format (int o bool o char)          ==> fn : int -> bool -> char -> string
 *  format (int o bool o char) 1 true #"x"
 *                                      ==> "1truex"
 *
 *  format (glue string "glue vs. " o string o glue int 42 o sp 5 o int)
 *         "ordinary text " 17
 *                                      ==> "glue vs. ordinary text 42     17" 
 *
 * Fragments can be padded, trimmed, or fitted to generate text pieces of 
 * specified sizes.  Padding/trimming/fitting may be nested.
 * The operations are parameterized by a place (left, center, right) and
 * a width. Padding never shrinks strings, trimming never extends
 * strings, and fitting is done as necessary by either padding or trimming.
 * Examples:
 *
 *  format (pad left 6 int) 1234        ==> "  1234"
 *  format (pad center 6 int) 1234      ==> " 1234 "
 *  format (pad right 6 int) 1234       ==> "1234  "
 *  format (trim left 2 int) 1234       ==> "34"
 *  format (trim center 2 int) 1234     ==> "23"
 *  format (trim right 2 int) 1234      ==> "12"
 *  format (fit left 3 int) 12          ==> " 12"
 *  format (fit left 3 int) 123         ==> "123"
 *  format (fit left 3 int) 1234        ==> "234"
 *
 * Nesting:
 *
 *  format (pad right 20 (int o pad left 10 real) o text "x") 12 22.3
 *                                      ==> "12      22.3        x"
 *)

signature FORMAT_COMB =
  sig

 (* We reveal "fragments" to be functions from abstract formats
  * to abstract formats.  This is to make sure we can use function
  * composition on them.
  *)
    type 'a format
    type ('a, 'b) fragment = 'a format -> 'b format

  (* Two primitive kinds of fragments:  Glue inserts some text
   * into the output without consuming an argument.  Elements
   * insert text corresponding to some (curried) argument into
   * the output:
   *)
    type 'a glue          = ('a, 'a) fragment
    type ('a, 't) element = ('a, 't -> 'a) fragment
    type 'a gg			(* abstract helper type *)

  (* Format execution... *)
  (*  1. Simple version, produce final result as a string: *)
    val format  : (string, 'a) fragment -> 'a

  (*  2. Complex version, take a receiver function that will
   *     be invoked with the final result.  The result is
   *     still in non-concatenated form at this time.
   *     (Internally, the combinators avoid string concatenation
   *      as long as there is no padding/trimming/fitting going on.)
   *)
    val format' : (string list -> 'b) -> ('b, 'a) fragment -> 'a

  (* Make a type-specific element given a toString function for this type *)
    val using : ('t -> string) -> ('a, 't) element

  (* Instantiate 'using' for a few types... *)
    val int     : ('a, int) element	(* using Int.toString *)
    val real    : ('a, real) element	(* using Real.toString *)
    val bool    : ('a, bool) element	(* using Bool.toString *)
    val string  : ('a, string) element	(* using (fn x => x) *)
    val string' : ('a, string) element	(* using String.toString *)
    val char    : ('a, char) element	(* using String.str *)
    val char'   : ('a, char) element	(* using Char.toString *)

  (* Parameterized elements... *)
    val int'  : StringCvt.radix   -> ('a, int) element  (* using (Int.fmt r) *)
    val real' : StringCvt.realfmt -> ('a, real) element	(* using(Real.fmt f) *)

  (* "polymorphic" elements *)
    val list   : ('a, 'x) element -> ('a, 'x list) element (* "[", ", ", "]" *)
    val option : ('a, 'x) element -> ('a, 'x option) element
    val seq    : (('x * 'a gg -> 'a gg) -> 'a gg -> 's -> 'a gg) -> (* foldr *)
		 'a glue ->	(* separator *)
		 ('a, 'x) element -> ('a, 's) element

  (* Generic "gluifier". *)
    val glue : ('a, 't) element -> 't -> 'a glue

  (* Inverse -- useful for writing extensions *)
    val elem : ('t -> 'a glue) -> ('a, 't) element

  (* Other glue... *)
    val nothing :           'a glue	(* null glue *)
    val text    : string -> 'a glue	(* constant text glue *)
    val sp      : int ->    'a glue	(* n spaces glue *)
    val nl      :           'a glue	(* newline glue *)
    val tab     :           'a glue	(* tabulator glue *)

  (* glue generator constructors *)
    val listg   : ('t -> 'a glue) -> ('t list -> 'a glue)
    val optiong : ('t -> 'a glue) -> ('t option -> 'a glue)

    val seqg   : (('x * 'a gg -> 'a gg) -> 'a gg -> 's -> 'a gg) -> (* foldr *)
		 'a glue ->	    (* separator *)
		 ('x -> 'a glue) ->  (* glue maker *)
		 's -> 'a glue	    (* glue maker for container *)


  (* "Places" say which side of a string to pad or trim... *)
    type place
    val left   : place
    val center : place
    val right  : place

  (* Pad, trim, or fit to size n the output corresponding to
   * a format fragment:
   *)
    val pad  : place -> int -> ('a, 't) fragment -> ('a, 't) fragment
    val trim : place -> int -> ('a, 't) fragment -> ('a, 't) fragment
    val fit  : place -> int -> ('a, 't) fragment -> ('a, 't) fragment

  (* specialized padding (left and right) *)
    val padl : int -> ('a, 't) fragment -> ('a, 't) fragment
    val padr : int -> ('a, 't) fragment -> ('a, 't) fragment

  end
