use "functional-trie.sml";

structure T = Trie;

(* the LSP method names *)
val names = [
        "textDocument/implementation",
        "textDocument/typeDefinition",
        "workspace/workspaceFolders",
        "workspace/configuration",
        "textDocument/documentColor",
        "textDocument/colorPresentation",
        "textDocument/foldingRange",
        "workspace/foldingRange/refresh",
        "textDocument/declaration",
        "textDocument/selectionRange",
        "window/workDoneProgress/create",
        "textDocument/prepareCallHierarchy",
        "callHierarchy/incomingCalls",
        "callHierarchy/outgoingCalls",
        "textDocument/semanticTokens/full",
        "textDocument/semanticTokens/full/delta",
        "textDocument/semanticTokens/range",
        "workspace/semanticTokens/refresh",
        "window/showDocument",
        "textDocument/linkedEditingRange",
        "workspace/willCreateFiles",
        "workspace/willRenameFiles",
        "workspace/willDeleteFiles",
        "textDocument/moniker",
        "textDocument/prepareTypeHierarchy",
        "typeHierarchy/supertypes",
        "typeHierarchy/subtypes",
        "textDocument/inlineValue",
        "workspace/inlineValue/refresh",
        "textDocument/inlayHint",
        "inlayHint/resolve",
        "workspace/inlayHint/refresh",
        "textDocument/diagnostic",
        "workspace/diagnostic",
        "workspace/diagnostic/refresh",
        "textDocument/inlineCompletion",
        "client/registerCapability",
        "client/unregisterCapability",
        "initialize",
        "shutdown",
        "window/showMessageRequest",
        "textDocument/willSaveWaitUntil",
        "textDocument/completion",
        "completionItem/resolve",
        "textDocument/hover",
        "textDocument/signatureHelp",
        "textDocument/definition",
        "textDocument/references",
        "textDocument/documentHighlight",
        "textDocument/documentSymbol",
        "textDocument/codeAction",
        "codeAction/resolve",
        "workspace/symbol",
        "workspaceSymbol/resolve",
        "textDocument/codeLens",
        "codeLens/resolve",
        "workspace/codeLens/refresh",
        "textDocument/documentLink",
        "documentLink/resolve",
        "textDocument/formatting",
        "textDocument/rangeFormatting",
        "textDocument/rangesFormatting",
        "textDocument/onTypeFormatting",
        "textDocument/rename",
        "textDocument/prepareRename",
        "workspace/executeCommand",
        "workspace/applyEdit",
        "workspace/didChangeWorkspaceFolders",
        "window/workDoneProgress/cancel",
        "workspace/didCreateFiles",
        "workspace/didRenameFiles",
        "workspace/didDeleteFiles",
        "notebookDocument/didOpen",
        "notebookDocument/didChange",
        "notebookDocument/didSave",
        "notebookDocument/didClose",
        "initialized",
        "exit",
        "workspace/didChangeConfiguration",
        "window/showMessage",
        "window/logMessage",
        "telemetry/event",
        "textDocument/didOpen",
        "textDocument/didChange",
        "textDocument/didClose",
        "textDocument/didSave",
        "textDocument/willSave",
        "workspace/didChangeWatchedFiles",
        "textDocument/publishDiagnostics",
        "$/setTrace",
        "$/logTrace",
        "$/cancelRequest",
        "$/progress"
      ];

val trie = let
      fun f (i, s, items) = (s, i)::items
      in
        T.make (List.foldli f [] names)
      end;

local
  fun one () = let
        fun lp (_, []) = ()
          | lp (i, x::xs) = (case T.find(trie, x)
               of SOME j => if (i <> j) then raise Fail "mismatch" else lp(i+1, xs)
                | NONE => raise Fail "missing"
              (* end case *))
        in
          lp (0, names)
        end
in
fun repeat 0 = () | repeat n = (one(); repeat (n-1))

fun timeIt n = let
      val () = SMLofNJ.Internals.GC.doGC 5
      val t0 = Time.now()
      val () = repeat n
      val secs = Time.toReal(Time.-(Time.now(), t0))
      val nOps = n * List.length names
      in
        print(concat[
            Int.toString nOps, " lookups in ",
            Real.toString secs, " seconds (",
            Int.toString(Real.round(real nOps / secs)), " per second)\n"
          ])
      end
end; (* local *)

