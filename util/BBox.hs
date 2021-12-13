{-# LANGUAGE FlexibleInstances #-}

module BBox (BoxBound, getBBox, bbox) where
import Control.Monad.State

-- #TODO extend this to multiple dimensions.  
--  wouldn't need the Maybe in the type signature, could just have empty list
--  each dimension is an element in the list with a min and max

-- ===============================================
--                  Bounding Box
-- ===============================================

-- I didn't make this "type BBox = State..."
--  so that users can't just specify any State action,
--  otherwise they could have "bbox x = put Nothing", 
--    and getBBox would fail
data BBox = BBox {
  getOp :: State (Maybe (Int,Int,Int,Int)) ()
}

class BoxBound a where
  bbox :: a -> BBox

-- Stretch the bounding box over each element in the array
instance BoxBound a => BoxBound [a] where
  bbox xs  = BBox $ mapM_ (getOp . bbox) xs

instance BoxBound (Int,Int) where
  bbox (x,y) = BBox $ do
    box <- get
    case box of 
      Nothing -> put $ Just (x,x,y,y)
      Just (minx,maxx,miny,maxy) -> put $ Just (min x minx, max x maxx, min y miny, max y maxy)

-- get the resulting bounding box as (min x, max x, min y, max y)
getBBox :: BoxBound a => a -> (Int,Int,Int,Int)
getBBox x = case run (bbox x) of
              Nothing -> error "Somehow got Nothing for bbox"
              Just result -> result


run :: BBox -> Maybe (Int,Int,Int,Int)
run (BBox op) = execState op Nothing
