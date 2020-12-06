module Day6 where

import           RIO

import qualified RIO.Text                      as Text
import qualified Data.Set                      as Set
import           Data.List.Split                ( splitOn )

part1 :: MonadIO f => f Int
part1 = sum . fmap (length . Set.unions) <$> parseInput

part2 :: MonadIO f => f Int
part2 = sum . fmap (length . foldl' Set.intersection allChars) <$> parseInput

allChars :: Set Char
allChars = Set.fromList ['a' .. 'z']

parseInput :: MonadIO f => f [[Set Char]]
parseInput =
  fmap (fmap (Set.fromList . Text.unpack))
    .   splitOn [""]
    .   Text.lines
    <$> readFileUtf8 "./input/day6.txt"
