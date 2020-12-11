module Day10 where

import           RIO

import qualified RIO.Text                      as T
import qualified Data.List                     as List
import qualified RIO.Map                       as Map
import           Data.List.Split

part1 :: MonadIO f => f (Maybe Integer)
part1 =
  (\m -> (*) <$> Map.lookup 1 m <*> Map.lookup 3 m)
    .   histogram
    .   differences
    .   mkLine
    <$> parseInput


part2 :: (Num b, MonadIO f) => f b
part2 =
  product
    .   map (permsFor . length)
    .   splitOn [3]
    .   differences
    .   mkLine
    <$> parseInput

permsFor :: (Eq a, Num a, Num p) => a -> p
permsFor 0 = 1
permsFor 1 = 1
permsFor 2 = 2
permsFor 3 = 4
permsFor 4 = 7
permsFor _ = error "I don't know!"

type Line = [Integer]

mkLine :: [Integer] -> Line
mkLine xs = 0 : sorted <> [List.last sorted + 3] where sorted = List.sort xs

differences :: Line -> Line
differences line = zipWith (-) (List.tail line) line

histogram :: (Foldable t, Ord k, Num a) => t k -> Map k a
histogram =
  List.foldl' (\m k -> Map.unionWith (+) m $ Map.singleton k 1) Map.empty

parseInput :: MonadIO m => m [Integer]
parseInput =
  mapMaybe (readMaybe . T.unpack) . T.lines <$> readFileUtf8 "./input/day10.txt"
