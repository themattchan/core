module Core.Utils where
import Data.Monoid
import Control.Lens.Wrapped (ala)
import Control.Applicative

alternatives :: Alternative f => [f a] -> f a
alternatives = ala Alt foldMap
