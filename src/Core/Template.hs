{-# LANGUAGE RecordWildCards, PatternGuards, ScopedTypeVariables #-}
module Core.Template where

import Control.Arrow
import Control.Monad
import Data.Monoid
import Data.List

import Core.Language
import Core.Utils
import Core.Parser

runProg :: String -> String
runProg = parseProgram >>> compile >>> eval >>> showResults

runFile :: FilePath -> IO ()
runFile = parseFile >=> compile >>> eval >>> showResults >>> putStrLn

data TiState = TiState { tiStack :: TiStack
                       , tiDump  :: TiDump
                       , tiHeap  :: TiHeap
                       , tiGlobals :: TiGlobals
                       , tiStats :: TiStats
                       } deriving (Show)

type TiStack = [Addr]
data TiDump = DummyTiDump deriving Show
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
step st@Heap{..} = dispatch (hLookup tiHeap (head tiStack)) where
  dispatch (NNum n) = numStep st n
  dispatch (NAp a1 a2) = apStep st a1 a2
  dispatch (NSupercomb sc args body) = scStep st args body

numStep _ _ = error "Number applied as a function!"

apStep st a1 a2 = st { tiStack = a1 : tiStack st }

scStep st sc_name arg_names body =
  st { tiStack = result_addr : drop (length arg_names + 1) (tiStack st)
     , tiHeap  = new_heap }
  where
    (new_heap, result_addr) = instantiate body (tiHeap st) env
    env = arg_bindings ++ tiGlobals st
    arg_bindings = zip arg_names (getArgs (tiHeap st) (tiStack st))

getArgs :: TiHeap -> TiStack -> [Addr]
getArgs heap (sc:stack) = map get_arg stack
  where
    get_arg addr = arg where (NAp fun arg) = hLookup heap addr

instantiate :: CoreExpr -> TiHeap -> [(Name,Addr)] -> (TiHeap,Addr)
instantiate (ENum n)  heap env = hAlloc heap (NNum n)
instantiate (EAp e1 e2) heap env = hAlloc heap2 (NAp a1 a2)
  where
    (heap1, a1) = instantiate e1 heap  env
    (heap2, a2) = instantiate e2 heap1 env
instantiate (EVar v) heap env = (heap, lookup' ("Unbound variable: " <> show v) v env)
instantiate ce _ _ = error "Can't instantiate expression type yet: " <> show ce


showResults :: [TiState] -> String
showResults = undefined
