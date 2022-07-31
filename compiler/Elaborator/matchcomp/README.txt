The new match compiler
----------------------

Files (out-of-date: earlier version)

-- "special" or "administrative" variables introduced by match compilation (== VarCon.var)
matchcomp/svar.sig
matchcomp/svar.sml

-- finite, ordered sets of rule numbers
matchcomp/rules.sig      
matchcomp/rules.sml      

-- basic types used by the match compiler: keys, paths, and-or trees, decision trees
matchcomp/mctypes.sml
matchcomp/mcutil.sml
matchcomp/mcprint.sml

-- the three main processes:
matchcomp/andor.sml         -- building the and-or tree for a match
matchcomp/or-queues.sml     -- building and using priority queues of OR nodes
matchcomp/decisiontree.sml  -- building the decision tree

-- virtual match "code constructions" producing absyn
matchcomp/vmcexp.sml    

-- environments used in "code generation" to build the match absyn
matchcomp/svarenv.sml
matchcomp/varenvmc.sml
matchcomp/varenvac.sml

-- the main function, building the and-or tree, the decision tree, and finally the absyn "code"
matchcomp/matchcomp.sml



Files in Elaborator/matchcomp (current/latest version)
------------------------------------------------------

mctypes.sml
rulesets.sml
svar.sml (redundant?  svar ~ VarCon.var)
mcutil.sml

andor.sml [structure AndOr]
  makeAndor -- construction of AND-OR trees

or-queues.sml [structure OR...]
  construction and use of OR-node priority queues

decisiontree.sml [structure DecisionTree]
  construction of decision tree (from AND-OR tree using priority queue
  of OR nodes

vmcexp.sml [structure VMCexp]
  interface for translating match to absyn code

matchcomp.sml [structure MatchComp]
  contains
  1. translation of rules, andor tree, decision tree, etc. to match
     absyn expression (genMatch, using genAndor, genDecTree)
  2. compilation of match rules to absyn (matchComp)
     [may recursively require translation of rhs expressions using
      transExp and transDec]
  3. Full absyn translation introducing generated match code (transExp, transDec)

  Subsumes former: transmatch.sml, matchcomp.sml



================================================================================

Incomplete Match Compiler bugs
------------------------------
MC does not handle all cases p | q where p and q are incompatible
"internally" (i.e. do not clash immediately at top-level of patterns).
Also nested OR patterns can cause problems, as in scc/t1a.sml
(from sparc-c-calls.sml).

Examples occur in:
* FLINT/opt/fcontract.sml (diff fcontract.sml saved.fcontract.sml,
  in function fcsDefault, line 955; also orpat/t4.sml)
* CPS/clos/closure.sml (function offset) (orpat/t0-3.sml)
* base/cm/tools/main/private-tools.sml (bvs/t*.sml)
* MLRISC/sparc/c-calls/sparc-c-calls.sml, line 227 (scc/t1a.sml)
* MLRISC/sparc/mltree/sparc.sml, line 370 (function addr; scc/t3.sml)

Simple workaround is to split the rules in question into two (or more)
rules with the same RHS as the original rule containing the problematic
OR-pattern(s).

Proper fix requires deep analysis of OR-pattern semantics and new
algorithm for dealing with OR patterns (probably/hopefully in
andor.sml).

One bug is that even for very simple OR patterns, the RHS is being
duplicated. See scc/t2a.sml.
