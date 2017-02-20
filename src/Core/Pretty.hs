module Core.Pretty where
import Text.PrettyPrint
import Core.Language

-- * Pretty printer
pprint :: CoreProgram -> String
pprint = undefined

ppExpr :: CoreExpr -> Doc
ppExpr (ENum n)    = int n
ppExpr (EVar v)    = text v
ppExpr (EAp e1 e2) = ppExpr e1 <> space <> ppAExpr e2

ppAExpr :: CoreExpr -> Doc
ppAExpr e | isAtomicExpr e = ppExpr e
          | otherwise      = parens (ppExpr e)
