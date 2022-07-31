(* Copyright (c) 1998 by Lucent Technologies *)
structure SimplifyAstExt:SIMPLIFYASTEXT = struct

  exception SimplifyAstExtExn of string

  fun fail s = raise (SimplifyAstExtExn s)

  fun simplifyExtDeclExt tables simplifiers ext = fail "no external declaration extensions defined"
  fun simplifyExpExt tables simplifiers ext = fail "no expression extensions defined"
  fun simplifyStmtExt tables simplifiers ext = fail "no statement extensions defined"
end
