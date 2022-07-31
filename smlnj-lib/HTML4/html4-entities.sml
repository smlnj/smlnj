(* html4-entities.sml
 *
 * COPYRIGHT (c) 2014 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure HTML4Entities =
  struct

    val nbsp = HTML4.ENTITY(Atom.atom "nbsp")
    val lt = HTML4.ENTITY(Atom.atom "lt")
    val gt = HTML4.ENTITY(Atom.atom "gt")
    val amp = HTML4.ENTITY(Atom.atom "amp")
    val quot = HTML4.ENTITY(Atom.atom "quot")
    val cent = HTML4.ENTITY(Atom.atom "cent")
    val pound = HTML4.ENTITY(Atom.atom "pound")
    val yen = HTML4.ENTITY(Atom.atom "yen")
    val euro = HTML4.ENTITY(Atom.atom "euro")
    val copy = HTML4.ENTITY(Atom.atom "copy")
    val reg = HTML4.ENTITY(Atom.atom "reg")

(* TODO: add the full set of HTML entities *)

  end

