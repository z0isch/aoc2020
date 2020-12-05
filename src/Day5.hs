module Day5 where

import           RIO

import qualified RIO.Text                      as Text
import qualified RIO.List                      as List
import qualified RIO.List.Partial              as List

part1 :: MonadIO f => f Int
part1 = List.head . List.sortOn Down . fmap (seatId . getSeat) <$> parseInput

part2 :: MonadIO f => f Int
part2 =
  seatId
    .   List.head
    .   mapMaybe oneSeatBetween
    .   pairSeats
    .   fmap getSeat
    <$> parseInput

parseInput :: MonadIO f => f [(Text, Text)]
parseInput =
  fmap (Text.splitAt 7) . Text.lines <$> readFileUtf8 "./input/day5.txt"

getRow :: Text -> Int
getRow = fst . Text.foldl' step (0, 127)

getCol :: Text -> Int
getCol = fst . Text.foldl' step (0, 7)

step :: (Int, Int) -> Char -> (Int, Int)
step (lo, hi) = \case
  'F' -> (lo, lo + mid)
  'B' -> (lo + mid + 1, hi)
  'L' -> (lo, lo + mid)
  'R' -> (lo + mid + 1, hi)
  _   -> error "Uh-oh"
  where mid = (hi - lo) `div` 2

getSeat :: (Text, Text) -> (Int, Int)
getSeat = bimap getRow getCol

seatId :: (Int, Int) -> Int
seatId (r, c) = (8 * r) + c

pairSeats :: [(Int, Int)] -> [((Int, Int), (Int, Int))]
pairSeats xs = let sorted = List.sort xs in zip sorted $ List.tail sorted

oneSeatBetween :: ((Int, Int), (Int, Int)) -> Maybe (Int, Int)
oneSeatBetween ((r1, c1), (r2, c2))
  | r1 == r2     = if c1 + 1 == c2 then Nothing else Just (r1, c1 + 1)
  | r1 + 1 == r2 = if c1 == 7 && c2 == 0 then Nothing else Just (r2, 0)
  | otherwise    = Nothing
