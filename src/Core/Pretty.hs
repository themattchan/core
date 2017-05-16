{-# LANGUAGE PatternGuards #-}
module Core.Pretty where
import Text.PrettyPrint
import Core.Language
import Core.Utils

ppVarList = hsep . map text

pprint :: CoreProgram -> String
pprint = render . fsep . ppProgram

ppProgram :: CoreProgram -> [Doc]
ppProgram = map ppScDefn

ppScDefn :: CoreScDefn -> Doc
ppScDefn (name, args, expr) =
  text name <+> ppVarList args <+> char '=' <+> ppExpr 1 expr

binops :: [(String, Int)]
binops = [("*", 1), ("/", 1)
         ,("+", 2), ("-", 2)] ++
         map (\o -> (o,3)) [ "<", ">", "==", "~=", ">=","<="] ++
         [("&",4)
         ,("|",5)]

parenPrec :: Bool -> (Doc -> Doc)
parenPrec b | b          = parens
            | otherwise  = id

ppExpr :: Int -> CoreExpr -> Doc
ppExpr _ (ENum n)    = int n
ppExpr _ (EVar v)    = text v
ppExpr _ (EConstr tag arity) = text "Pack" <> braces (int tag <> comma <> int arity)

ppExpr prec (EAp left@(EAp (EVar b) e1) e2)
  | Just p <-  b `lookup` binops
  = parenPrec (prec > p)
  $ ppExpr p e1 <+> text b <+> ppExpr p e2

ppExpr prec (EAp e1 e2) =
  let (p1,d1) = ppApp prec e1
      (p2,d2) = ppApp (prec+1) e2
  in parenPrec (p1<p2) d1 <+> parenPrec (p2<p1) d2 -- but now everything has full parens :(
  where
    ppApp prec e = (prec, ppExpr prec e)

ppExpr prec (ELet isRec defns expr)
  = hang (text (if isRec then "letrec" else "let"))
          3 (ppDefns defns) $+$
    text "in " <> ppExpr prec expr

ppExpr prec (ECase e alts)
  = hang caseof 2 (ppAlts alts)
    where
      caseof = text "case" <+> ppExpr prec e <+> text "of"
      ppAlts = vcat' . map ppAlter
      ppAlter (tag, vars, expr)
        = char '<' <> int tag <> char '>'
        <+> ppVarList vars
        <+> text "->" <+> ppExpr prec expr <+> semi

ppExpr prec (ELam args body) = char '\\' <> ppVarList args <> char '.' <+> ppExpr prec body

ppDefns :: [(Name, CoreExpr)] -> Doc
ppDefns = vcat' . punctuate semi . map ppDefn

ppDefn :: (Name, CoreExpr) -> Doc
ppDefn (name, expr) = ppScDefn (name, [], expr)
