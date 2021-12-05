import Control.Monad.State

import BBox

main = do
  print $ getBBox [(Point 0 0), (Point 10 15)]



data Point = Point {
  x :: Int,
  y :: Int
}

data Line = Line {
  start :: Point,
  end :: Point
}


instance BoxBound Point where
  bbox (Point x y) = makeBBox (x,y)

  -- This causes errors
  -- bbox (Point x y) = put Nothing

instance BoxBound Line where
  bbox (Line start end) = bbox start >> bbox end



