module Core.Pretty where
import Text.PrettyPrint
import Core.Language

hcat' = foldr ($+$) empty
ppVarList = hsep . map text

pprint :: CoreProgram -> String
pprint = render . fsep . ppProgram

ppProgram :: CoreProgram -> [Doc]
ppProgram = map ppScDefn

ppScDefn :: CoreScDefn -> Doc
ppScDefn (name, args, expr) =
  text name <+> ppVarList args <+> char '=' <+> ppExpr expr

ppExpr :: Int -> CoreExpr -> Doc
ppExpr _ (ENum n)    = int n
ppExpr _ (EVar v)    = text v
ppExpr _ (EConstr tag arity) = text "Pack" <> braces (int tag <> comma <> int arity)
ppExpr prec (EAp left@(EAp (EVar b) e1) e2)
  | b `elem` binops =
ppExpr prec (EAp e1 e2) = ppExpr prec e1 <+> ppAExpr prec e2
ppExpr prec (ELet isRec defns expr)
  = hang (text (if isRec then "letrec" else "let"))
          3 (ppDefns defns) $+$
    text "in " <> ppExpr expr
ppExpr prec (ECase e alts)
  = hang caseof 2 (ppAlts alts)
    where
      caseof = text "case" <+> ppExpr e <+> text "of"
      ppAlts = hcat' . map ppAlter
      ppAlter (tag, vars, expr)
        = char '<' <> int tag <> char '>'
        <+> ppVarList vars
        <+> text "->" <+> ppExpr expr <+> semi
ppExpr prec (ELam args body) = char '\\' <+> ppVarList args <+> char '.' <+> ppExpr body
--ppExpr e = ppAExpr e

-- ppAExpr :: CoreExpr -> Doc
-- ppAExpr e | isAtomicExpr e = ppExpr e
--           | otherwise      = parens (ppExpr e)

ppDefns :: [(Name, CoreExpr)] -> Doc
ppDefns = hcat' . punctuate semi . map ppDefn

ppDefn :: (Name, CoreExpr) -> Doc
ppDefn (name, expr) = ppScDefn (name, [], expr)
