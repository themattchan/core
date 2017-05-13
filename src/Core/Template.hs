module Core.Template where

import Core.Language
import Core.Utils

data TiState = TiState { tiStack :: TiStack
                       , tiDump  :: TiDump
                       , tiHeap  :: TiHeap
                       , tiGlobals :: TiGlobals
                       , tiStats :: TiStats
                       }

type TiStack = [Addr]
