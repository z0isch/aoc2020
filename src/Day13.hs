module Day13 where

import           RIO

import qualified RIO.Text                      as T
import qualified RIO.List                      as List
import qualified RIO.List.Partial              as List
import           Data.Maybe                     ( fromJust )
import           Data.Monoid

part1 :: MonadIO f => f (Product Integer)
part1 =
  bifoldMap Product Product
    .   List.head
    .   List.sortOn snd
    .   solve1
    .   second catMaybes
    .   parseInput
    <$> readFileUtf8 "./input/day13.txt"

part2 :: MonadIO f => f Integer
part2 =
  fst
    .   crt
    .   mapMaybe sequence
    .   zip [0, (-1) ..]
    .   snd
    .   parseInput
    <$> readFileUtf8 "./input/day13.txt"

solve1 :: (Integer, [Integer]) -> [(Integer, Integer)]
solve1 (t, bs) = fmap
  (   id
  &&& (\b -> b * (ceiling @Double $ (fromIntegral t /) $ fromIntegral b) - t)
  )
  bs

crt :: [(Integer, Integer)] -> (Integer, Integer)
crt = List.foldr1 go
 where
  go (r1, m1) (r2, m2) = (r `mod` m, m)
   where
    r = r2 + m2 * (r1 - r2) * (m2 `inv` m1)
    m = m2 * m1

inv :: Integer -> Integer -> Integer
a `inv` m = let (_, i, _) = egcd a m in i `mod` m

egcd :: Integer -> Integer -> (Integer, Integer, Integer)
egcd 0 b = (b, 0, 1)
egcd a b = (g, t - (b `div` a) * s, s) where (g, s, t) = egcd (b `mod` a) a

asInt :: Text -> Maybe Integer
asInt = readMaybe . T.unpack

parseInput :: Text -> (Integer, [Maybe Integer])
parseInput =
  (   (fromJust . asInt . List.head)
    &&& (fmap asInt . T.split (== ',') . List.last)
    )
    . T.lines

