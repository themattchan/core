module Core.Parser where
import Control.Exception (Exception(), throw)
import Data.Functor

import Text.Parsec hiding (spaces)
import Text.Parsec.Language
import qualified Text.Parsec.Token as P
import qualified Text.Parsec.Expr as P
import Text.Parsec.Char (digit)
import Text.Parsec.String (Parser())

import Core.Language

instance Exception ParseError

doParseProgram :: SourceName -> String -> CoreProgram
doParseProgram f = either throw id . parse pProgram f

parseFile :: FilePath -> IO CoreProgram
parseFile f = doParseProgram f <$> readFile f

parseProgram :: String -> CoreProgram
parseProgram = doParseProgram ""

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
spaces      = P.whiteSpace coreLexer
semi        = P.semi coreLexer

int = read <$> many1 digit

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

pCoreExpr :: Parser CoreExpr
pCoreExpr = choice [pLet, pCase, pLam, expr1] where
  expr1 = P.buildExpressionParser table term
  table = [ map binary ["*", "/"]
          , map binary ["+", "-"]
          , map binary relops
          , [ binary "&" ]
          , [ binary "|" ]
          ]

  -- App or single Aexpr
  term = foldl1 EAp <$> many1 pAexpr

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
    (i,j) <- braces $ (,) <$> int <* reservedOp "," <*> int
    return (EConstr i j)

  pAexpr = spaces *> choice [pVar, pNum, pCtor, parens pCoreExpr] <* spaces

  pNum  = ENum  <$> int
  pVar  = EVar  <$> identifier

  sequence1 = flip sepBy1 (reservedOp ";")
