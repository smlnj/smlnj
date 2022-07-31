(* uref-sig.sml
 *
 * Interface to UnionFind package.
 *
 * Author:
 *    Fritz Henglein
 *    DIKU, University of Copenhagen
 *    henglein@diku.dk
 *
 * DESCRIPTION
 *
 * Union/Find data type with ref-like interface.  A Union/Find structure 
 * consists of a type constructor 'a uref with operations for
 * making an element of 'a uref (make), getting the contents of
 * an element (!!), checking for equality of two elements (equal), and
 * for joining two elements (union).  uref is analogous to ref as
 * expressed in the following table:
 *
 * -------------------------------------------------------------------
 * type                  'a ref                'a uref
 * -------------------------------------------------------------------
 * introduction          ref                   uref
 * elimination           !                     !!
 * equality              =                     equal
 * updating              :=                    update
 * unioning                                    link, union, unify
 * -------------------------------------------------------------------
 *
 * The main difference between 'a ref and 'a uref is in the union
 * operation.  Without union 'a ref and 'a uref can be used
 * interchangebly.  An assignment to a reference changes only the
 * contents of the reference, but not the reference itself.  In
 * particular, any two pointers that were different (in the sense of the
 * equality predicate = returning false) before an assignment will still
 * be so.  Their contents may or may not be equal after the assignment,
 * though.  In contrast, applying the union operations (link, union,
 * unify) to two uref elements makes the two elements themselves
 * equal (in the sense of the predicate equal returning true).  As a
 * consequence their contents will also be identical: in the case of link
 * and union it will be the contents of one of the two unioned elements,
 * in the case of unify the contents is determined by a binary
 * function parameter.  The link, union, and unify functions return true
 * when the elements were previously NOT equal.
 *)

signature UREF =
  sig

    type 'a uref
	(* type of uref-elements with contents of type 'a *)  

    val uRef: 'a -> 'a uref
	(* uref x creates a new element with contents x *)

    val equal: 'a uref * 'a uref -> bool
	(* equal (e, e') returns true if and only if e and e' are either made by
	 * the same call to uref or if they have been unioned (see below).
	 *)

    val !! : 'a uref -> 'a
	(* !!e returns the contents of e. 
	 * Note: if 'a is an equality type then !!(uref x) = x, and 
	 * equal(uref (!!x), x) = false.
	 *)

    val update : 'a uref * 'a -> unit
	(* update(e, x) updates the contents of e to be x *)

    val unify : ('a * 'a -> 'a) -> 'a uref * 'a uref -> bool
	(* unify f (e, e') makes e and e' equal; if v and v' are the 
	 * contents of e and e', respectively, before unioning them, 
	 * then the contents of the unioned element is f(v,v').  Returns
	 * true, when elements were not equal prior to the call.
	 *)

    val union : 'a uref * 'a uref -> bool
	(* union (e, e') makes e and e' equal; the contents of the unioned
	 * element is the contents of one of e and e' before the union operation.
	 * After union(e, e') elements e and e' will be congruent in the
	 * sense that they are interchangeable in any context..  Returns
	 * true, when elements were not equal prior to the call.
	 *)

    val link : 'a uref * 'a uref -> bool
	(* link (e, e') makes e and e' equal; the contents of the linked
	 * element is the contents of e' before the link operation.  Returns
	 * true, when elements were not equal prior to the call.
	 *)

  end; (* UREF *)

