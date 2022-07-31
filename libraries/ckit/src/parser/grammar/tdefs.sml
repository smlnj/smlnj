(* Copyright (c) 1998 by Lucent Technologies *)

signature TYPEDEFS =
    sig
	val addTdef: string -> unit    (* a string as a typename in current scope *)
        val addNoTdef: string -> unit  (* a string is not a typename, 
					* may hide typenames of outer scopes *)
	val checkTdef: string -> bool  (* is this string a typename in current context ? *)
	val reset: unit -> unit        (* clear all tables, needed if you are doing many files *)
	val truncTo: int ref           (* limited-width names ? *)
	val pushScope: unit -> unit    (* entering a new scope in C *)
	val popScope: unit -> unit     (* exiting the last scope *)
    end 

(* We need a stack of tables to properly handle the scoping in typenames
 * Remember, there are four type of things competing in the namespace of
 * typenames:
 *   typenames, variables, functions and enum constants
 * Once you enter a new scope, reuse of these names can hide previous
 * uses.
 * Also note that struct field names do not redefine names within their scope
 * So, the following is legal:
 *    typedef int bar;
 *    struct h {
 *      bar bar;
 *      bar baz;
 *    };
 *)

structure TypeDefs : TYPEDEFS = 
struct
    structure ParseControl = Config.ParseControl

    exception NotTdef

    val truncTo: int ref = ref ParseControl.symbolLength;

    type item = bool (* true says typename, false says else *)
	
    val sayTdefs : bool ref = ref true;
	
    val tdefTable: (item AtomTable.hash_table list) ref = ref ([AtomTable.mkTable(1024, NotTdef)])

    fun checkTdef (str) =
      let 
	val s = substring(str,0,(!truncTo)) handle Substring => str
	val name = Atom.atom s
	fun lookup (n, nil) = NONE
	  | lookup (n, fst::rst) = 
	    (case (AtomTable.find (fst) n) of
	       (SOME x) => SOME(x)
	     | _ => (lookup(n,rst)))
      in
	case lookup(name, (!tdefTable)) of
	  (SOME true) => (if (!sayTdefs) then true else false)
	| (SOME false) => false
	| NONE => false
      end;


    fun pushScope () = (tdefTable := (AtomTable.mkTable(1024, NotTdef))::(!tdefTable); ())

    fun popScope () =   (* was just tl(!tdefTable), but caused problems with ml-yacc error correction *)
      (case (!tdefTable)
	 of [x] => ()  (* don't change *)
          | (_ :: l) => (tdefTable := l)
          | nil => ())
    (* don't change; but we are in trouble here! *)
	
    val errorCount = ref 0

    fun reset() = (tdefTable := [AtomTable.mkTable(1024, NotTdef)];
		   errorCount := 0)

    (* TBD: In the next two functions, it is an option to raise a syntax error,
     * if there is a redefinition in the same scope, i.e., the topmost table
     *)
      
    fun addTdef(str) = 
      let
	val s = substring(str,0,(!truncTo)) handle Substring => str
	val name = Atom.atom s
      in
	(* insert name in the top of tdefTable as a typename *)
	case !tdefTable of
	  x :: _ => AtomTable.insert x (name, true)
	| nil => (if !errorCount = 0
                  then print "Error: empty type def table (lexer), probably caused by syntax error" 
		        (* should be Error.error, but don't have an error stream handy. *)
		  else ();
		  errorCount := !errorCount + 1)
      end

    fun addNoTdef(str) = 
      let
	val s = substring(str,0,(!truncTo)) handle Substring => str
	val name = Atom.atom s
      in
	(* insert name in the top of tdefTable as not a typename *)
	case !tdefTable of
	  x :: _ => AtomTable.insert x (name, false)
	| nil => (if !errorCount = 0
                  then print "Error: empty type def table (lexer), probably caused by syntax error" 
		        (* should be Error.error, but don't have an error stream handy. *)
		  else ();
		  errorCount := !errorCount + 1)
      end
    
end

