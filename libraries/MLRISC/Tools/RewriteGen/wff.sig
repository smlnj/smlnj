signature WFF =
sig
   datatype wff = FALSE | TRUE | VAR of string
                | AND of wff * wff
                | OR of wff * wff
                | NOT of wff 

   val simplify   : wff -> wff
   val countNots  : wff -> int
   val countNots2 : wff -> int
   val allVars    : wff -> string list
end
