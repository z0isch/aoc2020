module Day2 where

import RIO hiding (many)

import qualified RIO.Map as Map
import qualified RIO.Text as Text
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

part1 :: MonadIO m => m Int
part1 = length . filter (uncurry isValid) <$> parseFile

part2 :: MonadIO m => m Int
part2 = length . filter (uncurry isValid2) <$> parseFile

type Range = (Int, Int)

data Policy = Policy { range :: Range, policyChar :: Char }
    deriving (Eq, Show, Generic)

type Parser = Parsec Void Text

rangeP :: Parser Range
rangeP = (,) <$> decimal <*> (char '-' *> decimal)

policyP :: Parser Policy
policyP = Policy <$> lexeme space1 rangeP <*> (letterChar <* char ':')

inputP :: Parser (Policy, Text)
inputP = (,) <$> lexeme space1 policyP <*> (Text.pack <$> many letterChar)

parseFile :: MonadIO m => m [(Policy, Text)]
parseFile =
  readFileUtf8 "./input/day2.txt"
    >>= either (throwString . errorBundlePretty) pure
    . parse (inputP `sepBy1` newline) "day2.txt"

histogram :: Text -> Map Char Int
histogram = Text.foldl (\m k -> Map.unionWith (+) m $ Map.singleton k 1) mempty

inRange :: Range -> Int -> Bool
inRange (x, y) input = input >= x && input <= y

isValid :: Policy -> Text -> Bool
isValid Policy { range, policyChar } input =
  inRange range $ fromMaybe 0 $ Map.lookup policyChar $ histogram input

isValid2 :: Policy -> Text -> Bool
isValid2 Policy { range, policyChar } input = uncurry xor
  $ bimap isChar isChar range
 where
  isChar i = Text.index input (i - 1) == policyChar
  xor True False = True
  xor False True = True
  xor _ _ = False
