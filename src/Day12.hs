module Day12 where

import           RIO

import qualified RIO.Text                      as T
import           Linear.Vector
import           Linear.V2
import           Linear.Epsilon
import           Linear.Matrix

data Boat = Boat{facing:: !(V2 Double), loc :: !(V2 Double)}
    deriving (Show, Eq)

type Inst = (Text, Double)

part1 :: MonadIO f => f Double
part1 = manhattenDist (V2 0 0) . loc . foldl' step initialBoat <$> parseInput

part2 :: MonadIO f => f Double
part2 = manhattenDist (V2 0 0) . loc . foldl' step2 initialBoat2 <$> parseInput

manhattenDist :: V2 Double -> V2 Double -> Double
manhattenDist v1 v2 = sum $ fmap abs v1 + fmap abs v2

initialBoat :: Boat
initialBoat = Boat { facing = V2 1 0, loc = V2 0 0 }

initialBoat2 :: Boat
initialBoat2 = Boat { facing = V2 10 1, loc = V2 0 0 }

mkZero :: Epsilon p => p -> p
mkZero x | nearZero x = 0
         | otherwise  = x

rot :: V2 Double -> Double -> V2 Double
rot v deg =
  let rad    = deg * pi / 180
      rotMat = fmap mkZero
        <$> V2 (V2 (cos rad) (-(sin rad))) (V2 (sin rad) (cos rad))
  in  v *! rotMat

step :: Boat -> Inst -> Boat
step b ("N", d) = b { loc = loc b + V2 0 d }
step b ("S", d) = b { loc = loc b + V2 0 (-d) }
step b ("E", d) = b { loc = loc b + V2 d 0 }
step b ("W", d) = b { loc = loc b + V2 (-d) 0 }
step b ("F", d) = b { loc = loc b + facing b ^* d }
step b ("L", d) = b { facing = rot (facing b) (-d) }
step b ("R", d) = b { facing = rot (facing b) d }
step _ _        = error "bad dir"

step2 :: Boat -> Inst -> Boat
step2 b ("N", d) = b { facing = facing b + V2 0 d }
step2 b ("S", d) = b { facing = facing b + V2 0 (-d) }
step2 b ("E", d) = b { facing = facing b + V2 d 0 }
step2 b ("W", d) = b { facing = facing b + V2 (-d) 0 }
step2 b ("F", d) = b { loc = loc b + facing b ^* d }
step2 b ("L", d) = b { facing = rot (facing b) (-d) }
step2 b ("R", d) = b { facing = rot (facing b) d }
step2 _ _        = error "bad dir"

parseInput :: MonadIO f => f [Inst]
parseInput =
  mapMaybe (sequence . (T.take 1 &&& (readMaybe . T.unpack . T.drop 1)))
    .   T.lines
    <$> readFileUtf8 "./input/day12.txt"
