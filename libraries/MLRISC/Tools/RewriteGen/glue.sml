(*
 * This file just links everything together
 *)

local
   structure AstUtil = MDLAstUtil(MDLAst)
   structure AstPP = MDLAstPrettyPrinter(AstUtil)
   structure AstRewriter = MDLAstRewriter(MDLAst)
   structure AstTrans = MDLAstTranslation
     (structure AstPP       = AstPP
      structure AstRewriter = AstRewriter
     )
   structure PolyGen = PolyGen
     (structure AstPP = AstPP
      structure AstTrans = AstTrans
     )
   structure Parser = MDLParserDriver
      (structure AstPP = AstPP
       val MDLmode = false
       val extraCells = []
      )
in

structure RewriterGen = RewriteGen
     (structure AstPP = AstPP
      structure AstRewriter = AstRewriter
      structure PolyGen = PolyGen
      structure AstTrans = AstTrans
      structure Parser = Parser
     )
end
