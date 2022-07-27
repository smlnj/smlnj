(* item.sml
 *
 * COPYRIGHT (c) 2006
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (http://www.cs.uchicago.edu/~adrassi)
 * All rights reserved.
 *
 * Utility code for the item datatype.
 *)

structure Item =
  struct

    structure S = LLKSpec

    fun sym (S.ITEM {sym = s, ...}) = s
    val name = Preitem.name o sym
    val toString = Preitem.toString o sym
    val listToString = Preitem.listToString o (map sym)

    fun id (S.ITEM {id, ...}) = id
    fun compare (item1, item2) = Int.compare(id item1, id item2)
    fun same pair = (case compare pair
		      of EQUAL => true
		       | _ => false)

    fun nt itm = (case sym itm
          of S.NONTERM (nt, _) => SOME nt
	   | S.POSCLOS nt => SOME nt
	   | S.CLOS nt => SOME nt
	   | S.OPT nt => SOME nt
	   | S.TOK _ => NONE
         (* end case *))

  (* return the SET of bound names to the left of an item in a production,
   * and the SET of the formal parameters available to the item.
   * we use sets rather than lists because names might be 
   * repeated in the bindings.
   *)
    fun bindingsLeftOf (item, prod) = let
          fun lhs (LLKSpec.PROD {lhs, ...}) = lhs
          fun items (LLKSpec.PROD {rhs, ...}) = !rhs
          fun itemBindings (LLKSpec.PROD {rhsBindings, ...}) = 
	        (fn (x,_) => x) (ListPair.unzip rhsBindings)
          fun leftOf ([], accum) = raise Fail "BUG: leftOf on empty list"
	    | leftOf ((i, name)::is, accum) = 
	        if same (i, item) then accum
		else leftOf (is, AtomSet.add(accum, Atom.atom name))
	  val (parentBindings, formals) = case Nonterm.parent (lhs prod)
		of NONE => (AtomSet.empty,
			    AtomSet.addList (AtomSet.empty, Nonterm.formals (lhs prod)))
		 | SOME prod' => let
		     fun isProdItm (itm) = (case nt itm
                           of SOME nt => Nonterm.same (nt, lhs prod)
			    | NONE => false)
		     val prodItm = valOf (List.find isProdItm (items prod'))
		     in
		       bindingsLeftOf (prodItm, prod')
		     end
          in
            (leftOf (ListPair.zip (items prod, itemBindings prod), 
		     parentBindings),
	     formals)
          end

    structure Set = RedBlackSetFn (
      struct
	type ord_key = S.item
	val compare = compare
      end)

    structure Map = RedBlackMapFn (
      struct
	type ord_key = S.item
	val compare = compare
      end)

  end
