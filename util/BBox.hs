module BBox (BoxBound, getBBox, makeBBox, bbox) where
import Control.Monad.State

-- ===============================================
--                  Bounding Box
-- ===============================================

type BBox = State (Maybe (Int,Int,Int,Int)) ()

-- want to restrict this so that users can only specify bbox using makeBBox
class BoxBound a where
  bbox :: a -> BBox

instance BoxBound a => BoxBound [a] where
  bbox = mapM_ bbox

makeBBox :: (Int,Int) -> BBox
makeBBox (x,y) = do
    box <- get
    case box of 
      Nothing -> put $ Just (x,x,y,y)
      Just (minx,maxx,miny,maxy) -> put $ Just (min x minx, max x maxx, min y miny, max y maxy)

getBBox :: BoxBound a => a -> (Int,Int,Int,Int)
getBBox x = case execState (bbox x) Nothing of
              Nothing -> error "Somehow got Nothing for bbox"
              Just result -> result
