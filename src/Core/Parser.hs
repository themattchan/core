module Core.Parser where
import Data.Functor

import Text.Parsec
import Text.Parsec.Language
import qualified Text.Parsec.Token as P
import qualified Text.Parsec.Expr as P
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String (parseFromFile, Parser(..))

import Core.Language
import Core.Utils

-- * Lexing and utils

coreDef = haskellStyle
  { P.reservedNames = [ "let", "letrec", "case", "in", "of", "Pack" ]
  , P.reservedOpNames = [ "=", ",", ";","->","<-", "\\", "."]
  }
relops = ["==", "~=", ">=","<="]
arithops = [ "+","-","*","/"]
bitops = ["&","|"]

coreLexer   = P.makeTokenParser coreDef
parens      = P.parens coreLexer
braces      = P.braces coreLexer
identifier  = P.identifier coreLexer
reserved    = P.reserved coreLexer
reservedOp  = P.reservedOp coreLexer
integer     = P.integer coreLexer
symbol      = P.symbol coreLexer
operator    = P.operator coreLexer
spaces      = P.whiteSpace
semi        = P.semi coreLexer

int = fromIntegral <$> integer

binary op = P.Infix e P.AssocLeft where
  e = do { o <- symbol op; return (\x y -> EAp (EAp (EVar o) x) y) }

-- * Parser

pProgram :: Parser CoreProgram
pProgram = P.semiSep1 coreLexer pSc

-- Supercombinators
pSc :: Parser CoreScDefn
pSc = do
  name <- identifier
  args <- many identifier
  reservedOp "="
  body <- pCoreExpr
  return (name, args, body)

data PartialExpr = NoOp | FoundOp Name CoreExpr

assembleOp e1 NoOp = e1
assembleOp e1 (FoundOp op e2) = EAp (EAp (EVar op) e1) e2

tryOp opks = alternatives (map (uncurry try1) opks) <|> return NoOp
  where try1 op k = FoundOp <$> op <*> k

pCoreExpr :: Parser CoreExpr
pCoreExpr = alternatives [try pLet, try pCase, try pLam, expr1] where
  expr1 = assembleOp <$> expr2 <*> tryOp [(string "|", expr1)]
  expr2 = assembleOp <$> expr3 <*> tryOp [(string "&", expr2)]
  expr3 = assembleOp <$> expr4 <*> tryOp [(alternatives (string <$> relops), expr4)]
  expr4 = assembleOp <$> expr5 <*> tryOp [(string "+", expr4), (string "+", expr5)]
  expr5 = assembleOp <$> expr6 <*> tryOp [(string "*", expr5), (string "/", expr6)]

  -- App or single Aexpr
  expr6 = foldl1 EAp <$> many1 (try pAexpr)

  pLam = do
    reservedOp "\\"
    params <- many1 identifier
    reservedOp "."
    expr <- pCoreExpr
    return (ELam params expr)

  pCase = do
    reserved "case"
    e <- pCoreExpr
    reserved "of"
    alts <- sequence1 pAlt1
    return (ECase e alts)
    where
      pAlt1 = do
        i <- reservedOp "<" *> int <* reservedOp ">"
        vars <- many identifier
        reservedOp "->"
        expr <- pCoreExpr
        return (i, vars, expr)

  pLet  = do
    isrec <- try (reserved "let" $> False) <|> (reserved "letrec" $> True)
    binds <- sequence1 bind1
    reserved "in"
    expr <- pCoreExpr
    return (ELet isrec binds expr)
    where
      bind1 = do
        name <- identifier
        reservedOp "="
        expr <- pCoreExpr
        return (name, expr)

  pCtor = do
    reserved "Pack"
    (i,j) <- braces $ (,) <$> int <*> (reservedOp "," >> int)
    return (EConstr i j)

  pAexpr = alternatives [pVar, pNum, pCtor, parens pCoreExpr]

  pNum  = ENum  <$> int
  pVar  = EVar  <$> identifier

  sequence1 = flip sepBy1 (reservedOp ";")
