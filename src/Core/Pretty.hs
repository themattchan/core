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

ppExpr :: CoreExpr -> Doc
ppExpr (ENum n)    = int n
ppExpr (EVar v)    = text v
-- TODO Ex 1.8 precedence for builtin operators
ppExpr (EAp e1 e2) = ppExpr e1 <+> ppAExpr e2
ppExpr (ELet isRec defns expr)
  = hang (text (if isRec then "letrec" else "let")
          3 (ppDefns defns) $+$
    text "in " <> ppExpr expr
ppExpr (ECase e alts)
  = hang caseof 2 (ppAlts alts)
    where
      caseof = text "case" <+> ppExpr e <+> text "of"
      ppAlts = hcat' . map ppAlter
      ppAlter (tag, vars, expr)
        = char '<' <> int tag <> char '>'
        <+> ppVarList vars
        <+> text "->" <+> ppExpr expr <+> semi
ppExpr (ELam args body) = char '\' <+> ppVarList args <+> char '.' <+> ppExpr body
ppExpr e = ppAExpr e

ppAExpr :: CoreExpr -> Doc
ppAExpr (EConstr tag arity) = text "Pack" <> braces (int tag <> comma <> int arity)
ppAExpr e | isAtomicExpr e = ppExpr e
          | otherwise      = parens (ppExpr e)

ppDefns :: [(Name, CoreExpr)] -> Doc
ppDefns = hcat' . punctuate semi . map ppDefn

ppDefn :: (Name, CoreExpr) -> Doc
ppDefn (name, expr) = ppScDefn (name, [], expr)
