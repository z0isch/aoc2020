module Day17 where

import           RIO

import           Linear.V3
import           Linear.V4
import qualified RIO.Map                       as Map
import qualified RIO.Set                       as Set
import qualified RIO.Text                      as T
import qualified RIO.Text.Partial              as T
import qualified RIO.List                      as List
import qualified RIO.List.Partial              as List

part1 :: MonadIO f => f Int
part1 = solve . parseInput <$> readFileUtf8 "./input/day17.txt"

part2 :: MonadIO f => f Int
part2 =
  solve
    .   Set.fromList
    .   fmap (\(V3 x y z) -> V4 x y z 0)
    .   Set.toList
    .   parseInput
    <$> readFileUtf8 "./input/day17.txt"

solve :: (Ord (t Int), Traversable t) => Set (t Int) -> Int
solve = length . List.head . List.drop 6 . List.iterate step

step :: (Ord (t Int), Traversable t) => Set (t Int) -> Set (t Int)
step vs = Set.fromList $ mapMaybe (runActive vs) (Set.toList vs) <> mapMaybe
  (runInactive vs)
  (Set.toList $ nearbyInactiveNeighbors vs)

nearbyInactiveNeighbors
  :: (Ord (t Int), Traversable t) => Set (t Int) -> Set (t Int)
nearbyInactiveNeighbors vs =
  Set.difference (Set.fromList $ concatMap neighbors vs) vs

runInactive
  :: (Ord (t Int), Traversable t) => Set (t Int) -> t Int -> Maybe (t Int)
runInactive vs v | numActiveNeighbors vs v == 3 = Just v
                 | otherwise                    = Nothing

runActive
  :: (Ord (t Int), Traversable t) => Set (t Int) -> t Int -> Maybe (t Int)
runActive vs v
  | numActiveNeighbors vs v == 2 || numActiveNeighbors vs v == 3 = Just v
  | otherwise = Nothing

numActiveNeighbors
  :: (Ord (t Int), Traversable t) => Set (t Int) -> t Int -> Int
numActiveNeighbors vs v = length $ filter (`Set.member` vs) $ neighbors v

neighbors :: (Eq (t Int), Traversable t) => t Int -> [t Int]
neighbors v = List.filter (/= v) $ traverse (\c -> [c - 1 .. c + 1]) v

parseInput :: (Ord a, Num a, Enum a) => Text -> Set (V3 a)
parseInput =
  Map.keysSet
    . Map.filter (== '#')
    . Map.fromList
    . concat
    . zipWith
        (\y vs ->
          T.foldl' (\cs@((V3 x _ _, _) : _) v -> (V3 (x + 1) y 0, v) : cs)
                   [(V3 0 y 0, T.head vs)]
            $ T.tail vs
        )
        [0 ..]
    . T.lines
