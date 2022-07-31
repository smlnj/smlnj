(* Copyright (c) 1998 by Lucent Technologies *)

structure CopyAstExt = struct
  fun copyExprExt (copyExp,copyStmt,copyExtDecl) aidctx ext = ext
  fun copyStmtExt (copyExp,copyStmt,copyExtDecl) aidctx ext = ext
  fun copyExtDeclExt (copyExp,copyStmt,copyExtDecl) aidctx ext = ext
end

