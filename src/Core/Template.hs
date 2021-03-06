-- | Chapter 2: Template instantiation
module Core.Template where

import Control.Arrow ((>>>))
import Control.Monad
import Data.Semigroup (Semigroup(..), Max(..))
import Data.Monoid hiding ((<>))
import Data.List
import GHC.Generics (Generic)
import Generics.Deriving.Monoid (memptydefault, mappenddefault)

import Text.PrettyPrint hiding ((<>))

import Control.Lens (to, (&), (<&>), (%~), (^.), (+~), (.~), (<>~), (^?!))
import Control.Lens.TH

import Core.Language
import Core.Utils hiding (heapStats)
import Core.Parser

--------------------------------------------------------------------------------
-- * Types

data TiState = TiState
  { tiStateStack   :: TiStack
  -- ^ stack of addresses, each identifying a node.
  -- The stack forms the spine of the expr under evaluation.
  , tiStateDump    :: TiDump
  -- ^ state of spine stack prior to the evaluation of a strict primitive.
  , tiStateHeap    :: TiHeap
  -- ^ collection of tagged nodes
  , tiStateGlobals :: TiGlobals
  -- ^ node addresses of supercombinators and primitives.
  , tiStateStats   :: TiStats
  } deriving (Show)

type TiStack = [Addr]
data TiDump = DummyTiDump deriving Show

initialTiDump :: TiDump
initialTiDump = DummyTiDump

type TiHeap = Heap Node

data Node
  = NAp Addr Addr
  | NSupercomb Name [Name] CoreExpr
  | NNum Int
  deriving (Show, Eq)

type TiGlobals =  [(Name,Addr)]

data TiStats = TiStats
  { tiStatsScReductions   :: Sum Int
  , tiStatsPrimReductions :: Sum Int
  , tiStatsHeapStats      :: HeapStats
  , tiStatsSteps          :: Sum Int
  , tiStatsMaxStackDepth  :: Max Int
  } deriving (Show, Generic)
instance Semigroup TiStats where
  (<>) = mappenddefault
instance Monoid TiStats where
  mempty = memptydefault
  mappend = (<>)

makeLensesWith camelCaseFields  ''TiState
makeLensesWith camelCaseFields  ''TiStats

--------------------------------------------------------------------------------
-- * Runners

runProg :: String -> Either ParseError String
runProg = parseProgram >>> fmap (compile >>> eval >>> showResults)

runFile :: FilePath -> IO ()
runFile = parseFile >=> (compile >>> eval >>> showResults >>> putStrLn)

reduceToNormalForm :: String -> Either ParseError Node
reduceToNormalForm = parseProgram >>> fmap (compile >>> eval >>> last >>> getNode)
  where
    getNode :: TiState -> Node
    getNode st = hLookup (st^.heap) (st ^. stack^?! to head)

--------------------------------------------------------------------------------
-- * Step 1: Compile

compile :: CoreProgram -> TiState
compile p = TiState initialStack initialTiDump initialHeap globals mempty
  where
    sc_defs                = p <> preludeDefs
    (initialHeap, globals) = buildInitialHeap sc_defs
    initialStack           = [address_of_main]
    address_of_main        = lookupErr ("main is not defined") "main" globals

buildInitialHeap :: CoreProgram -> (TiHeap, [(String, Addr)])
buildInitialHeap = mapAccumL allocateSc hInitial where
  allocateSc heap (name, args, body)
    = (\addr -> (name, addr)) <$> hAlloc heap (NSupercomb name args body)

--------------------------------------------------------------------------------
-- * Step 2: Evaluate

eval :: TiState -> [TiState]
eval state = state : rest_states where
  rest_states | tiFinal state = []
              | otherwise     = eval next_state
  next_state = doAdmin (step state)

doAdmin :: TiState -> TiState
doAdmin st = st & stats.steps <>~ Sum 1
                & stats.maxStackDepth <>~ Max (length (st ^. stack))
                & stats.heapStats .~ (st^.heap.stats)
-- TODO count prim/supercombo reductions

tiFinal :: TiState -> Bool
tiFinal (TiState stack dump heap globals stats)
  | [sole_addr] <- stack = isDataNode (hLookup heap sole_addr)
  | null stack           = error ("Empty stack!")
  | otherwise            = False

isDataNode :: Node -> Bool
isDataNode (NNum _) = True
isDataNode _        = False

step :: TiState -> TiState
step st@(TiState stack _ heap _ _) = dispatch (hLookup heap (head stack))
  where
    dispatch (NNum n)                  = numStep st n
    dispatch (NAp a1 a2)               = apStep st a1 a2
    dispatch (NSupercomb sc args body) = scStep st sc args body

numStep :: TiState -> Int -> TiState
numStep _ _ = error "Number applied as a function!"

apStep :: TiState -> Addr -> Addr -> TiState
apStep st a1 a2 = st & stack %~ (a1 :)

-- To apply a supercombinator...
scStep :: TiState -> Name -> [Name]  -> CoreExpr -> TiState
scStep st sc_name arg_names body =
  st & stack .~ result_addr : rest
     & heap  .~ new_heap
  where
    -- get lhs of binding from the stack
    (sc_and_actuals, rest)  = splitAt (length arg_names + 1) (st ^. stack)
    -- build environment (map from names to addrs) from binders and globals
    env                     = arg_bindings ++ st ^. globals
    -- find heap addresses of formal parameters and bind to arg_names
    actual_params           = getArgs (st ^. heap) sc_and_actuals
    arg_bindings
      | length (tail sc_and_actuals) == length arg_names
      = zip arg_names actual_params
      | otherwise
      = error "Supercombinator applied to too few arguments"
    -- finally, instantiate the expression in the heap
    (new_heap, result_addr) = instantiate body (st ^. heap) env

-- Given a stack (with supercombinator on top), find heap addresses of all the
-- arguments of all function application nodes.
getArgs :: TiHeap -> TiStack -> [Addr]
getArgs heap (sc:stack) = map get_arg stack
  where
    get_arg addr = let (NAp fun arg) = hLookup heap addr in arg

instantiate :: CoreExpr      -- ^ supercombinator body
            -> TiHeap        -- ^ heap before instantiation
            -> [(Name,Addr)] -- ^ name -> addr environment
            -> (TiHeap,Addr) -- ^ heap after instantiation, address at root of instance
instantiate (ENum n)    heap env = hAlloc heap  (NNum n)

instantiate (EAp e1 e2) heap env = hAlloc heap2 (NAp a1 a2)
  where
    (heap1, a1) = instantiate e1 heap  env
    (heap2, a2) = instantiate e2 heap1 env

instantiate (EVar v)    heap env =
  (heap, lookupErr ("Unbound variable: " <> show v) v env)

instantiate (ELet isRec bindings body) heap env
  = let (heap', env') =
          foldl (\(accHeap,accEnv) (var,expr) ->
               let (accHeap', addr) = instantiate expr accHeap
                                          ( if isRec then env' else env)
                   accEnv' = (var,addr) : accEnv
               in (accHeap', accEnv')
            ) (heap,env) bindings
    in instantiate body heap' env'

instantiate ce _ _ = error $ "Can't instantiate expression type yet: " <> show ce

--------------------------------------------------------------------------------
-- * Step 3: Print results

showResults :: [TiState] -> String
showResults states = render . vcat . punctuate (text "\n") $ results
  where
    results = map showState states <> [ showStats (last states) ]

showState :: TiState -> Doc
showState TiState{..} = showStack tiStateHeap tiStateStack $+$
                        showHeap  tiStateHeap

showHeap :: TiHeap -> Doc
showHeap = (text "Heap" <+>) . brackets
         . hcat . punctuate (comma <> space)
         . map showAddrD . hAddresses

showStack :: TiHeap -> TiStack -> Doc
showStack heap stack = text "Stk" <+> brackets (nest 2 (vcat items))
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
showStats st =
  mconcat [ text "\n\n"
          , text "Total number of steps = ", int (st ^. stats.steps.to getSum)
          ]
