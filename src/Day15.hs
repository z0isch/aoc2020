{-# LANGUAGE TupleSections #-}
module Day15 where

import           RIO

import qualified RIO.HashMap                   as HashMap
import qualified RIO.List                      as List
import qualified RIO.List.Partial              as List

part1 :: (Int, Int)
part1 = solve 2020 input

part2 :: (Int, Int)
part2 = solve 30000000 input

data GameState = GameState
  { spoken :: !(HashMap Int (Int, Maybe Int))
  , mostRecent :: !(Int,Int)
  }
  deriving (Eq,Show)

solve :: Int -> [Int] -> (Int, Int)
solve num i =
  mostRecent $ List.head $ drop (num - length i) $ List.iterate step $ initial i

step :: GameState -> GameState
step state = state
  { spoken     = HashMap.insertWith (\(n, _) (s, _) -> (n, Just s))
                                    newSpoken
                                    (nextRound, Nothing)
                   $ spoken state
  , mostRecent = (newSpoken, nextRound)
  }
 where
  nextRound = lastRound + 1
  newSpoken = case HashMap.lookup mostRecentSpoke (spoken state) of
    Nothing           -> 0
    Just (_, Nothing) -> 0
    Just (lastTimeSpoken, Just timeBeforeLast) ->
      lastTimeSpoken - timeBeforeLast
  (mostRecentSpoke, lastRound) = mostRecent state

initial :: [Int] -> GameState
initial xs = GameState
  { spoken     = HashMap.fromList $ zip xs $ map (, Nothing) [1 ..]
  , mostRecent = (List.last xs, length xs)
  }

input :: Num a => [a]
input = [20, 0, 1, 11, 6, 3]
