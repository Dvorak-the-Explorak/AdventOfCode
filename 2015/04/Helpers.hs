module Utils where

import Data.List (foldl')
import Data.Function ((&))

chain :: [a -> a] -> a -> a
chain = flip $ foldl' (&)
