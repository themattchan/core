module Core.Utils where
import Data.Function
import Data.Maybe
import Data.Monoid hiding ((<>))
import Data.Semigroup
import Text.PrettyPrint hiding ((<>))
import Control.Lens.TH
import Generics.Deriving.Monoid (memptydefault, mappenddefault)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- * Misc utilities

vcat' :: [Doc] -> Doc
vcat' = foldr ($$) empty

besides :: [Doc] -> [Doc] -> Doc
besides xs ys = uncurry ((<+>) `on` vcat')
              . unzip
              $ (zip `on` (++ pad)) xs ys
  where
    d = abs (length xs - length ys)
    pad = replicate d (text "")

--------------------------------------------------------------------------------
-- * Appendix A: Heap data type and associated functions

type Addr = Int
data Heap a = Heap { heapSize :: Int, heapFree :: [Addr], heapCts :: [(Addr,a)]
                   } deriving Show

data HeapStats = HeapStats
  { heapStatsAllocs  :: Sum Int
  , heapStatsUpdates :: Sum Int
  , heapStatsRemoves :: Sum Int
  } deriving (Show, Generic)
instance Semigroup HeapStats where
  (<>) = mappenddefault
instance Monoid HeapStats where
  mempty = memptydefault
  mappend = (<>)

makeLensesWith camelCaseFields  ''HeapStats

hInitial :: Heap a
hInitial = Heap 0 [1..] []

hAlloc :: Heap a -> a -> (Heap a, Addr)
hAlloc (Heap s (next:free) cts) n = (Heap (s+1) free ((next,n):cts), next)
hAlloc _ _ = error "hAlloc: heap full"

hUpdate :: Heap a -> Addr -> a -> Heap a
hUpdate h a n = h { heapCts = (a,n) : remove (heapCts h) a }

hFree :: Heap a -> Addr -> Heap a
hFree h a = h { heapSize = heapSize h -1
              , heapFree = a : heapFree h
              , heapCts  = remove (heapCts h) a
              }

hLookup :: Heap a -> Addr -> a
hLookup h a = lookupErr err a (heapCts h)
  where err = "can't find node " <> showAddr a <> " in heap"

lookupErr :: Eq a => String -> a -> [(a, b)] -> b
lookupErr err a xs = fromMaybe (error err) (lookup a xs)

hAddresses :: Heap a -> [Addr]
hAddresses = map fst . heapCts

hNull :: Addr
hNull = 0

hIsNull :: Addr -> Bool
hIsNull = (0 ==)

showAddr :: Addr -> String
showAddr = ("#" <>) . show

remove :: [(Addr,a)] -> Addr -> [(Addr,a)]
remove [] a = error ("Attempt to update or free nonexistent address " <> showAddr a)
remove ((a',n):cts) a | a == a' = cts
                      | a /= a' = (a',n) : remove cts a
