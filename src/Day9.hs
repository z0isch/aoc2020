module Day9 where

import           RIO

import qualified RIO.Text                      as T
import qualified RIO.List                      as List
import qualified RIO.List.Partial              as List
import           Data.Monoid
import           Control.Lens                   ( _2 )

part1 :: MonadIO f => f (Maybe Integer)
part1 =
  fmap (view _2)
    .   List.headMaybe
    .   List.dropWhile isValid
    .   List.iterate step
    .   init
    <$> parseInput

part1Answer :: Integer
part1Answer = 466456641

part2 :: MonadIO f => f (Maybe (Sum Integer))
part2 =
  fmap (bifoldMap Sum Sum . (List.head &&& List.last . List.sort))
    .   List.headMaybe
    .   mapMaybe (validBlock part1Answer)
    .   List.tails
    <$> parseInput

validBlock :: Integer -> [Integer] -> Maybe [Integer]
validBlock sumTo xs = case maybeAnswer of
  Just possible | sum possible == sumTo -> Just possible
  _ -> Nothing
 where
  maybeAnswer = List.headMaybe $ List.dropWhile ((< sumTo) . sum) $ zipWith
    List.take
    [1 ..]
    (List.repeat xs)

init :: [Integer] -> ([Integer], Integer, [Integer])
init ns =
  let (prev, x : xs) = List.splitAt 25 ns in (List.reverse prev, x, xs)

step :: ([Integer], Integer, [Integer]) -> ([Integer], Integer, [Integer])
step (prev, curr, n : next) = (curr : prev, n, next)

isValid :: ([Integer], Integer, [Integer]) -> Bool
isValid (prev, curr, _) = curr `elem` pairs
 where
  pairs = [ x + y | x <- List.take 25 prev, y <- List.take 25 prev, x /= y ]

parseInput :: MonadIO m => m [Integer]
parseInput =
  mapMaybe (readMaybe . T.unpack) . T.lines <$> readFileUtf8 "./input/day9.txt"
