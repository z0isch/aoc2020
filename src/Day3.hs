module Day3 where

import RIO

import RIO.Text as Text
import RIO.List.Partial as List
import RIO.List as List
import Linear.V2

part1 :: MonadIO f => f Int
part1 = numTrees (V2 3 1) <$> parseInput

part2 :: MonadIO f => f Int
part2 = parseInput <&> \input ->
  product $ fmap (`numTrees` input) [V2 1 1, V2 3 1, V2 5 1, V2 7 1, V2 1 2]

type Grid = [[Bool]]

numTrees :: V2 Int -> [[Bool]] -> Int
numTrees step input = snd $ List.last $ List.scanl'
  (\(c, trees) _ ->
    let
      c'@(V2 x y) = c + step
      trees' =
        if y < List.length grid && grid !! y !! x then trees + 1 else trees
    in (c', trees')
  )
  (V2 0 0, 0)
  grid
  where grid = mkInfiniteGrid input

mkInfiniteGrid :: [[Bool]] -> Grid
mkInfiniteGrid = fmap List.cycle

isTree :: Char -> Bool
isTree '#' = True
isTree _ = False

parseInput :: MonadIO f => f [[Bool]]
parseInput =
  fmap (Text.foldl' (\xs c -> xs ++ [isTree c]) [])
    . Text.lines
    <$> readFileUtf8 "./input/day3.txt"
