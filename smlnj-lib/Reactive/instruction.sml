(* instruction.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * An AST representation of reactive scripts.
 *)

structure Instruction =
  struct

    datatype 'a config
      = posConfig of 'a
      | negConfig of 'a
      | orConfig of ('a config * 'a config)
      | andConfig of ('a config * 'a config)

    type signal = Atom.atom

    datatype 'ctxt instr
      = || of ('ctxt instr * 'ctxt instr)		(* merge *)
      | & of ('ctxt instr * 'ctxt instr)		(* sequencing *)
      | nothing						(* nop *)
      | stop						(* stop execution *)
      | suspend						(* suspend execution *)
      | action of 'ctxt -> unit				(* an atomic action *)
      | exec of 'ctxt -> {stop : unit -> unit, done : unit -> bool}
      | ifThenElse of (('ctxt -> bool) * 'ctxt instr * 'ctxt instr)
      | repeat of (int * 'ctxt instr)			(* repeat loop *)
      | loop of 'ctxt instr				(* infinite loop *)
      | close of 'ctxt instr
      | signal of (signal * 'ctxt instr)		(* define a signal *)
      | rebind of (signal * signal * 'ctxt instr)	(* rename a signal *)
      | when of (signal config * 'ctxt instr * 'ctxt instr)
      | trapWith of (signal config * 'ctxt instr * 'ctxt instr)
      | emit of signal					(* generate a signal *)
      | await of signal config				(* wait for a signal *)

  end;
