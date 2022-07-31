(* html4-utils.sml
 *
 * COPYRIGHT (c) 2014 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Defines a set of utility data types and functions for the HTML 4 parser.
 *)

structure HTML4Utils = struct

(* ____________________________________________________________
   Parse trees
 *)

datatype 'a parsetree = Nd of Atom.atom * 'a parsetree list
                      | Lf of 'a

(* ____________________________________________________________
   Data structure and utilities for element tokens
 *)

type tag_payload = string * (Atom.atom * string option) list

fun attrToStr (name, NONE) = Atom.toString name
  | attrToStr (name, SOME a_val) = String.concat [Atom.toString name, "=",
                                                  a_val]

fun attrsToStr attrs = String.concatWith " " (map attrToStr attrs)

fun payloadToStr (payload, []) = payload
  | payloadToStr (_, attrs as (attr :: _)) = attrsToStr attrs

val getAttrs : tag_payload -> (Atom.atom * string option) list = #2

(* ____________________________________________________________
   Streams
 *)

datatype 'a stream = StreamCons of 'a * (unit -> 'a stream)
                   | StreamNil

exception EmptyStream

fun stream_hd (StreamCons (v, _)) = v
  | stream_hd StreamNil = raise EmptyStream

fun stream_tl (StreamCons (_, rst)) = rst()
  | stream_tl StreamNil = raise EmptyStream

fun stream_nth (stream, 0) = stream_hd stream
  | stream_nth (stream, idx) = stream_nth(stream_tl stream, idx - 1)

fun stream_empty StreamNil = true
  | stream_empty (StreamCons _) = false

fun stream_concat (StreamNil, stream2) = stream2
  | stream_concat (StreamCons(hdval, tl_thunk), stream2) =
    StreamCons(hdval, fn () => (stream_concat (tl_thunk(), stream2)))

fun stream_concatl [] = StreamNil
  | stream_concatl (StreamNil :: streams) = stream_concatl streams
  | stream_concatl ((StreamCons(hdval, tl_thunk)) :: streams) =
    StreamCons(hdval, fn () => (stream_concatl ((tl_thunk())::streams)))

(* stream_concatt() - Special concat that allows a stream thunk to be
appended to the tail of a stream. *)

fun stream_concatt (StreamNil, tl_thunk2) = tl_thunk2()
  | stream_concatt (StreamCons(hdval, tl_thunk1), tl_thunk2) =
    StreamCons(hdval, fn () => stream_concatt (tl_thunk1(), tl_thunk2))

fun stream_map mapfn StreamNil = StreamNil
  | stream_map mapfn (StreamCons (hdval, tl_thunk)) =
    StreamCons(mapfn hdval, fn () => (stream_map mapfn (tl_thunk())))

(* stream_maps() - Full blown transduction from one kind of stream to
another, where the mapper returns a stream.  This allows one to zero
and one to many mappings, as opposed to stream_map() which only allows
one to one maps. *)

fun stream_maps mapsfn instrm =
    (case instrm of
         StreamNil => StreamNil
       | StreamCons(crnt_hd, tl_thunk) =>
         let val outstrm_front = mapsfn crnt_hd
             fun tl_thunk' () = stream_maps mapsfn (tl_thunk ())
         in stream_concatt(outstrm_front, tl_thunk') end)

fun stream_app appfn StreamNil = ()
  | stream_app appfn (StreamCons (hdval, tl_thunk)) =
    (appfn hdval; stream_app appfn (tl_thunk()));

fun stream_filter pred StreamNil = StreamNil
  | stream_filter pred (StreamCons (hdval, tl_thunk)) =
    if pred hdval then StreamCons(hdval,
                               fn () => (stream_filter pred (tl_thunk())))
    else stream_filter pred (tl_thunk())

fun stream_foldl foldlfn acc StreamNil = acc
  | stream_foldl foldlfn acc (StreamCons(hd_val, tl_thunk)) =
    stream_foldl foldlfn (foldlfn(hd_val, acc)) (tl_thunk())

fun stream_singleton soleval = StreamCons(soleval, fn () => StreamNil)

fun stream_inf infval = StreamCons(infval, fn () => (stream_inf infval))

fun stream_fromList [] = StreamNil
  | stream_fromList (elem::elems) =
    StreamCons(elem, fn () => stream_fromList elems)

(* ____________________________________________________________
   Parse tree streams
 *)

datatype 'a parsevisitation = EnterNT of Atom.atom
                            | ExitNT of Atom.atom
                            | VisitT of 'a

fun visitationToString _ (EnterNT ntAtom) =
      concat["entry of ", Atom.toString ntAtom, " nonterminal"]
  | visitationToString _ (ExitNT ntAtom) =
      concat["exit of ", Atom.toString ntAtom, " nonterminal"]
  | visitationToString termToString (VisitT terminal) =
      concat["vistation of ", termToString terminal, " terminal"]

fun visitationSame _ (EnterNT ntAtom, EnterNT ntAtom') = Atom.same(ntAtom, ntAtom')
  | visitationSame _ (ExitNT ntAtom, ExitNT ntAtom') = Atom.same(ntAtom, ntAtom')
  | visitationSame termSame (VisitT term, VisitT term') = termSame(term, term')
  | visitationSame _ _ = false

fun parsetreeToVisitationStream (node as (Nd (ntAtom, children))) =
    let fun tl_thunk () =
            let val children' = map parsetreeToVisitationStream children
            in
                stream_concat(stream_concatl children',
                              stream_singleton (ExitNT ntAtom))
            end
    in
        StreamCons(EnterNT ntAtom, tl_thunk)
    end
  | parsetreeToVisitationStream (node as (Lf payload)) =
    StreamCons(VisitT payload, fn () => StreamNil)

fun visitationStreamToParsetree strm =
    let fun handleVisit (EnterNT _, (spine, peers)) =
            (peers :: spine, [])
          | handleVisit (ExitNT ntAtom, ((peers :: spine), children')) =
            (spine, (Nd (ntAtom, rev children')) :: peers)
          | handleVisit (VisitT term, (spine, peers)) =
            (spine, (Lf term) :: peers)
        val (_, result :: _) = stream_foldl handleVisit ([], []) strm
    in result end

fun parsetreeStreamMapT maptfn =
    let fun transduce StreamNil = StreamNil
          | transduce (StreamCons(crnt_hd, tl_thunk)) =
            let val hd' = case crnt_hd of
                              VisitT term => VisitT (maptfn term)
                            | _ => crnt_hd
                fun tl_thunk' () = transduce (tl_thunk ())
            in StreamCons(hd', tl_thunk') end
    in transduce end

(* parsetreeStreamMapTStream(): given a function that maps from
terminals to a parse tree visitation stream, do a map over an existing
visitation stream.  This should be useful for mapping some placeholder
token into a synthetic nonterminal or list of terminals. *)

fun parsetreeStreamMapTStream (guardfn, maptsfn) =
    let fun transduce StreamNil = StreamNil
          | transduce (StreamCons(crnt_hd, tl_thunk)) =
            let fun tl_thunk' () = transduce (tl_thunk ())
            in case crnt_hd of
                   VisitT term => if (guardfn term)
                                  then stream_concatt(maptsfn term,
                                                     tl_thunk')
                                  else StreamCons(crnt_hd, tl_thunk')
                 | _ => StreamCons(crnt_hd, tl_thunk')
            end
    in transduce end

fun mkParsetreeStreamToString termToString strm =
    let fun handleVisit (EnterNT ntAtom, (indent, outs)) =
            (String.concat [indent, " "],
             (String.concat [indent, Atom.toString ntAtom, "\n"] :: outs))
          | handleVisit (ExitNT ntAtom, (indent, outs)) =
            (String.extract(indent, 1, NONE), outs)
          | handleVisit (VisitT term, (indent, outs)) =
            (indent, String.concat [indent, termToString term, "\n"] :: outs)
        val (_, outs) = stream_foldl handleVisit ("", []) strm
    in String.concat(rev outs) end

end

(* ______________________________________________________________________
   End of html4-utils.sml
   ______________________________________________________________________ *)
