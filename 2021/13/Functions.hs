{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Functions where

import qualified Data.HashSet as Set
import Data.Hashable


import Control.Lens (Lens, Lens', lens, view, over)
import Control.Lens.Tuple (_1, _2)

import Utils
import BBox
import Types

import Debug.Trace
ttrace x = trace (show x) x



printGrid :: Set Point -> IO ()
printGrid points = do
    let grid = getGrid points
    mapM putStrLn $ map concat grid
    return ()

getGrid :: Set Point -> [[String]]
getGrid points = map (map paintCoord) gridCoords
  where 
    (minx, maxx, miny, maxy) = getBBox points
    gridCoords = [[Point (x,y) | x <- [minx..maxx]] | y <- [miny..maxy]]
    
    paintCoord p = if p `Set.member` points 
                    then "██" 
                    else "░░"


doFolds :: [Fold] -> Set Point -> Set Point
-- doFolds points folds = foldl' (\ps f -> doFold f ps) points folds
-- doFolds = flip $ foldl' $ flip doFold
doFolds folds = chain $ (map doFold) folds

doFold :: Fold -> Set Point -> Set Point
doFold (Vertical x) = foldOn (_p._1) x
doFold (Horizontal y) = foldOn (_p._2) y

-- 
foldOn :: Lens' Point Int -> Int -> Set Point -> Set Point
foldOn l pos points = Set.union low high'
  where
    low = Set.filter ((<pos) . view l) points
    high = Set.filter ((>pos) . view l) points

    high' = Set.map (over l $ \val -> 2*pos - val) high

