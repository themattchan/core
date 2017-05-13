module Core.Template where

import Control.Arrow
import Control.Monad
import Data.Monoid

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
                       }

type TiStack = [Addr]
data TiDump = DummyTiDump
initialTiDump = DummyTiDump

type TiHeap = Heap Node

data Node = NAp Addr Addr
          | NSupercomb Name [Name] CoreExpr
          | NNum Int

type TiGlobals =  [(Name,Addr)]

type TiStats = Int
tiStatInitial = 0
tiStatIncSteps s = s+1
tiStatGetSteps s = s

compile :: CoreProgram -> TiState
compile p = TiState initialStack initialTiDump initialHeap globals tiStatInitial
  where
    sc_defs = p <> preludeDefs
    (initialHeap, globals) = buildInitialHeap sc_defs
    initialStack = [address_of_main]
    address_of_main = lookup' ("main is not defined") "main" globals

buildInitialHeap :: CoreProgram -> (TiHeap, [(String, CoreScDefn)])
buildInitialHeap p = (heap, scBinds) where
  scBinds = map (\sc@(n,_,_) -> (n,sc)) p


eval :: TiState -> [TiState]
eval = undefined
showResults :: [TiState] -> String
showResults = undefined

updateStats st f = st { tiStats = f (tiStats st) }
