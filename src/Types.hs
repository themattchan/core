module Core.Language where

type Name = String

-- A core lang program is a list of defns
type Program a = [ScDefn a]
type CoreProgram = Program Name

-- A supercombinator (toplevel) definition is the name, args, and body
type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn Name

-- Cases in case expressions
type Alter a = (Int, [a], Expr a)
type CoreAlt = Alter Name

type CoreExpr = Expr Name

-- The main expr data type
data Expr a
  = EVar Name				-- Variables
  | ENum Int				-- Numbers
  | EConstr Int Int			-- Construtor tag arity
  | EAp (Expr a) (Expr a)	-- Appliations
  | ELet					-- Let(rec) expressions
      Bool					--   boolean with True = recursive,
      [(a, Expr a)]			--   Definitions
      (Expr a)				--   Body of let(re)
  | ECase					-- Case expression
      (Expr a)				--   Expression to scrutinise
      [Alter a]				--   Alternatives
  | ELam [a] [(Expr a)]		-- Lambda abstractions
  deriving (Show, Eq)

bindersOf :: [(a,b)] -> [a]
bindersOf = map fst

rhssOf :: [(a,b)] -> [b]
rhssOf = map snd

isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar _) = True
isAtomicExpr (ENum _) = True
isAtomicExpr _        = False

preludeDefs :: CoreProgram
preludeDefs
  = [ ("I", ["x"], EVar "x"),
    , ("K", ["x","y"], EVar "x")
    , ("K1",["x","y"], EVar "y")
    , ("S", ["f","g","x"], EAp (EAp (EVar "f") (EVar "x"))
                               (EAp (EVar "g") (EVar "x")))
    , ("compose", ["f","g","x"], EAp (EVar "f")
	                                   (EAp (EVar "g") (EVar "x")))
    , ("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f"))
	]

pprint :: CoreProgram -> String
