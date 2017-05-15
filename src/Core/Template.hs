{-# LANGUAGE RecordWildCards, PatternGuards, ScopedTypeVariables #-}
module Core.Template where

import Control.Arrow ((>>>))
import Control.Monad
import Data.Monoid
import Data.List

import Text.PrettyPrint hiding ((<>))

import Core.Language
import Core.Utils
import Core.Parser

runProg :: String -> String
runProg = parseProgram >>> compile >>> eval >>> showResults

runFile :: FilePath -> IO ()
runFile = parseFile >=> compile >>> eval >>> showResults >>> putStrLn

--------------------------------------------------------------------------------
-- * Types

data TiState = TiState
  { tiStack   :: TiStack
  , tiDump    :: TiDump
  , tiHeap    :: TiHeap
  , tiGlobals :: TiGlobals
  , tiStats   :: TiStats
  } deriving (Show)

type TiStack = [Addr]
data TiDump = DummyTiDump deriving Show

initialTiDump :: TiDump
initialTiDump = DummyTiDump

type TiHeap = Heap Node

data Node = NAp Addr Addr
          | NSupercomb Name [Name] CoreExpr
          | NNum Int
          deriving (Show)

type TiGlobals =  [(Name,Addr)]

type TiStats = Int
tiStatInitial = 0
tiStatIncSteps s = s+1
tiStatGetSteps s = s
updateStats f st = st { tiStats = f (tiStats st) }

--------------------------------------------------------------------------------
-- * Step 1: Compile

compile :: CoreProgram -> TiState
compile p = TiState initialStack initialTiDump initialHeap globals tiStatInitial
  where
    sc_defs = p <> preludeDefs
    (initialHeap, globals) = buildInitialHeap sc_defs
    initialStack = [address_of_main]
    address_of_main = lookup' ("main is not defined") "main" globals

buildInitialHeap :: CoreProgram -> (TiHeap, [(String, Addr)])
buildInitialHeap = mapAccumL go hInitial where
  go heap (name, args, body) = (\addr -> (name, addr)) <$> hAlloc heap (NSupercomb name args body)

--------------------------------------------------------------------------------
-- * Step 2: Evaluate

eval :: TiState -> [TiState]
eval state = state : rest_states where
  rest_states | tiFinal state = []
              | otherwise = eval next_state
  next_state = doAdmin (step state)

doAdmin :: TiState -> TiState
doAdmin = updateStats tiStatIncSteps

tiFinal :: TiState -> Bool
tiFinal TiState{..}
  | [sole_addr] <- tiStack = isDataNode (hLookup tiHeap sole_addr)
  | [] <- tiStack = error ("Empty stack!")
  | otherwise = False

isDataNode :: Node -> Bool
isDataNode (NNum _) = True
isDataNode _        = False

step :: TiState -> TiState
step st@TiState{..} = dispatch (hLookup tiHeap (head tiStack)) where
  dispatch (NNum n) = numStep st n
  dispatch (NAp a1 a2) = apStep st a1 a2
  dispatch (NSupercomb sc args body) = scStep st sc args body

numStep :: TiState -> Int -> TiState
numStep _ _ = error "Number applied as a function!"

apStep :: TiState -> Addr -> Addr -> TiState
apStep st a1 a2 = st { tiStack = a1 : tiStack st }

scStep :: TiState -> Name -> [Name]  -> CoreExpr -> TiState
scStep st sc_name arg_names body =
  st { tiStack = result_addr : rest
     , tiHeap  = new_heap }
  where
    (sc_and_actuals, rest) = splitAt (length arg_names + 1) (tiStack st)
    (new_heap, result_addr) = instantiate body (tiHeap st) env
    env = arg_bindings ++ tiGlobals st
    actual_params = getArgs (tiHeap st) sc_and_actuals
    arg_bindings
      | length (tail sc_and_actuals) == length arg_names = zip arg_names actual_params
      | otherwise = error $ "Supercombinator applied to too few arguments"


getArgs :: TiHeap -> TiStack -> [Addr]
getArgs heap (sc:stack) = map get_arg stack
  where
    get_arg addr = let (NAp fun arg) = hLookup heap addr in arg

instantiate :: CoreExpr -> TiHeap -> [(Name,Addr)] -> (TiHeap,Addr)
instantiate (ENum n)    heap env = hAlloc heap  (NNum n)
instantiate (EAp e1 e2) heap env = hAlloc heap2 (NAp a1 a2)
  where
    (heap1, a1) = instantiate e1 heap  env
    (heap2, a2) = instantiate e2 heap1 env
instantiate (EVar v)    heap env = (heap, lookup' ("Unbound variable: " <> show v) v env)
instantiate ce _ _ = error $ "Can't instantiate expression type yet: " <> show ce

--------------------------------------------------------------------------------
-- * Step 3: Print results

showResults :: [TiState] -> String
showResults states = render . vcat' . punctuate (text "\n") $ results where
  results = map showState states <> [ showStats (last states) ]

showState :: TiState -> Doc
showState TiState{..} = showStack tiHeap tiStack $+$ showHeap tiHeap

showHeap :: TiHeap -> Doc
showHeap = (text "Heap" <+>) . brackets
         . hcat . punctuate (comma <> space)
         . map showAddrD . hAddresses

showStack :: TiHeap -> TiStack -> Doc
showStack heap stack = text "Stk" <+> brackets (nest 2 (vcat' items))
  where
    items = map showStackItem stack
    showStackItem addr = mconcat [ showFWAddr addr, text ": "
                                 , showStkNode heap (hLookup heap addr)
                                 ]

showStkNode :: TiHeap -> Node -> Doc
showStkNode heap (NAp fun_addr arg_addr) =
  mconcat [ text "NAp"
          , space, showFWAddr fun_addr
          , space, showFWAddr arg_addr
          , space, parens (showNode (hLookup heap arg_addr))
          ]
showStkNode _heap node = showNode node

showNode :: Node -> Doc
showNode (NAp a1 a2) =
  mconcat [ text "NAp ", showAddrD a1
          , space      , showAddrD a2
          ]
showNode (NSupercomb name _args _body) =
  text "NSupercomb" <+> text name
showNode (NNum n) = text "NNum" <+> int n

showAddrD :: Addr -> Doc
showAddrD addr = text (showAddr addr)

-- Show address in field of width 4
showFWAddr :: Addr -> Doc
showFWAddr addr = pad <> text a
  where a   = showAddr addr
        pad = mconcat (replicate (4 - length a) space)

showStats :: TiState -> Doc
showStats TiState{..} =
  mconcat [ text "\n\n"
          , text "Total number of steps = ", int (tiStatGetSteps tiStats)
          ]
