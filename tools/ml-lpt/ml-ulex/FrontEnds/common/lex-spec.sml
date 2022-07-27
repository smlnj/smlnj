(* lex-spec.sml
 *
 * COPYRIGHT (c) 2005
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (adrassi@gmail.com)
 * All rights reserved.
 *
 * Input specification to ml-ulex
 *)

structure LexSpec =
  struct

    datatype clamp = CLAMP127 | CLAMP255 | NO_CLAMP

    type action = string
    type rule_spec = AtomSet.set option * RegExp.re
    type rule = rule_spec * action
    type eof_rule = string * action

    datatype config = Conf of {
	structName : string,
	header : string,
	arg : string,
	startStates : AtomSet.set,
	clamp : clamp			(* not used yet *)
      }

    datatype spec = Spec of {
        decls : string,
	conf : config,
	rules : rule list,
	eofRules : eof_rule list
      }

    fun mkConfig () = Conf {
            structName = "", header = "", arg = "",
	    startStates = AtomSet.empty,
	    clamp = CLAMP127
	  }

    fun updStartStates (conf, new) = let
          val Conf {structName, header, arg, startStates, clamp} = conf
	  in Conf {
	         structName = structName,
		 header = header,
		 arg = arg,
	         startStates = new,
		 clamp = clamp
 	       }
          end

    fun updHeader (conf, new) = let
          val Conf {structName, header, startStates, arg, clamp} = conf
(* FIXME: we should be reporting an error instead of raising an exception here! *)
	  val _ = if String.size structName > 0
		  then raise Fail "Cannot have both %structure and %header"
		  else ()
	  in Conf {
	         structName = structName,
		 header = new,
		 arg = arg,
	         startStates = startStates,
		 clamp = clamp
	       }
	  end

    fun updStructName (conf, new) = let
          val Conf {structName, header, startStates, arg, clamp} = conf
(* FIXME: we should be reporting an error instead of raising an exception here! *)
	  val _ = if String.size header > 0
		  then raise Fail "Cannot have both %structure and %header"
		  else ()
	  in Conf {
	         structName = new,
		 header = header,
		 arg = arg,
	         startStates = startStates,
		 clamp = clamp
	       }
	  end

    fun updArg (conf, new) = let
          val Conf {structName, header, startStates, arg, clamp} = conf
	  in Conf {
	         structName = structName,
		 header = header,
		 arg = new,
	         startStates = startStates,
		 clamp = clamp
	       }
	  end

    fun updClamp (conf, new) = let
          val Conf {structName, header, arg, startStates, clamp} = conf
	  in Conf {
	         structName = structName,
		 header = header,
		 arg = arg,
	         startStates = startStates,
		 clamp = new
	       }
          end

    fun mkSpec() = Spec {decls = "", conf = mkConfig(), rules = [], eofRules = []}

    fun addRule (spec, new) = let
          val Spec {decls, conf, rules, eofRules} = spec
          in
            Spec {decls = decls, conf = conf,
		  rules = rules @ [new], eofRules = eofRules}
          end

    fun addEOFRuleFront (spec, new) = let
          val Spec {decls, conf, rules, eofRules} = spec
          in
            Spec {decls = decls, conf = conf,
		  rules = rules, eofRules = new::eofRules}
          end

    fun addEOFRule (spec, new) = let
          val Spec {decls, conf, rules, eofRules} = spec
          in
            Spec {decls = decls, conf = conf,
		  rules = rules, eofRules = eofRules @ [new]}
          end

    fun getConf (Spec {conf, ...}) = conf
    fun updConf (spec, new) = let
          val Spec {decls, conf, rules, eofRules} = spec
          in
            Spec {decls = decls, conf = new, rules = rules, eofRules = eofRules}
          end

    fun updDecls (spec, new) = let
          val Spec {decls, conf, rules, eofRules} = spec
          in
            Spec {decls = new, conf = conf, rules = rules, eofRules = eofRules}
          end

    fun emptyActions (spec) = let
          val Spec {decls, conf, rules, eofRules} = spec
          val Conf {structName, header, arg, startStates, clamp} = conf
          val conf' = Conf {
		 structName = "", header = "", arg = "", clamp = clamp,
		 startStates = startStates
	       }
	  fun clearRule (rspec, action) = (rspec, "()")
          in Spec {
	         decls = "fun eof() = ()\ntype lex_result = unit",
		 conf = conf',
		 rules = List.map clearRule rules,
		 eofRules = List.map clearRule eofRules
               }
          end

  end
