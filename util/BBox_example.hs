import Control.Monad.State

import BBox

main = do
  print $ getBBox [Line (Point 0 0) (Point 10 15), Line (Point (-5) 7) (Point 3 2)]

data Point = Point {
  x :: Int,
  y :: Int
}

data Line = Line {
  start :: Point,
  end :: Point
}


instance BoxBound Point where
  bbox (Point x y) = bbox (x,y)

instance BoxBound Line where
  bbox (Line start end) = bbox [start, end]
  -- bbox (Line start end) = put Nothing



