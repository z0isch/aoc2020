{-# LANGUAGE TupleSections #-}
module Day11 where

import           RIO
import qualified RIO.Text                      as T
import qualified RIO.Map                       as Map
import qualified RIO.Map.Partial               as Map
import qualified RIO.List                      as List
import           Linear.V2
import qualified Data.List                     as List

type Chart = Map (V2 Int) Char

part1 :: MonadIO f => f (Maybe Int)
part1 = fmap (numOcc . snd) . doIt step1 <$> parseInput

part2 :: MonadIO f => f (Maybe Int)
part2 = fmap (numOcc . snd) . doIt step2 <$> parseInput

numOcc :: Chart -> Int
numOcc = length . Map.filter (== '#')

doIt :: Eq a => (a -> a) -> a -> Maybe (a, a)
doIt f startingState =
  let states = List.iterate f startingState
  in  List.headMaybe $ List.dropWhile (uncurry (/=)) $ zip states
                                                           (List.tail states)
step1 :: Chart -> Chart
step1 chart = Map.mapWithKey bar chart
 where
  neighborsOf = mapMaybe (`Map.lookup` chart) . neighbors
  bar c 'L' = if elem '#' $ neighborsOf c then 'L' else '#'
  bar c _   = if length (filter (== '#') $ neighborsOf c) >= 4 then 'L' else '#'

step2 :: Chart -> Chart
step2 chart = Map.mapWithKey bar chart
 where
  neighborsOf c =
    mapMaybe (`Map.lookup` chart) $ mapMaybe (castRay chart c) dirs
  bar c 'L' = if elem '#' $ neighborsOf c then 'L' else '#'
  bar c _   = if length (filter (== '#') $ neighborsOf c) >= 5 then 'L' else '#'

castRay :: Chart -> V2 Int -> V2 Int -> Maybe (V2 Int)
castRay chart c dir =
  List.headMaybe
    $ dropWhile (\c' -> Map.notMember c' chart && inBounds c')
    $ map (+ c)
    $ List.scanl' (+) dir (List.repeat dir)
 where
  inBounds (V2 x y) = x >= 0 && y >= 0 && x <= maxX && y <= maxY
  (V2 maxX maxY, _) = Map.findMax chart

dirs :: [V2 Int]
dirs =
  [ V2 1    0
  , V2 0    1
  , V2 1    1
  , V2 (-1) 1
  , V2 (-1) 0
  , V2 0    (-1)
  , V2 (-1) (-1)
  , V2 1    (-1)
  ]

neighbors :: V2 Int -> [V2 Int]
neighbors c1 = [ c1 + c2 | c2 <- dirs ]

parseInput :: MonadIO f => f Chart
parseInput =
  Map.fromList
    .   filter ((/= '.') . snd)
    .   concat
    .   zipWith (\y -> fmap (\(x, c) -> (V2 x y, c))) [0 ..]
    .   fmap (zip [0 ..] . T.unpack)
    .   T.lines
    <$> readFileUtf8 "./input/day11.txt"
