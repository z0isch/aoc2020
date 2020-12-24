module Day19 where

import           RIO                     hiding ( some )

import qualified RIO.Map                       as Map
import qualified RIO.Map.Partial               as Map
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

part1 :: MonadIO m => m Int
part1 = do
  (rules, possibles) <- parseFile
  pure $ length $ filter (\p -> lookupRule rules p $ rules Map.! 0) possibles

part2 :: MonadIO m => m Int
part2 = do
  (rules, possibles) <- parseFile
  let rules2 = Map.insert 11 (Left [[42, 31], [42, 11, 31]])
        $ Map.insert 8 (Left [[42], [42, 8]]) rules
  pure $ length $ filter (\p -> lookupRule rules2 p $ rules Map.! 0) possibles

lookupRule
  :: Map Int (Either [[Int]] Char) -> String -> Either [[Int]] Char -> Bool
lookupRule rules m = any (any null) . go [m]
 where
  go :: [String] -> Either [[Int]] Char -> [[String]]
  go []        _ = []
  go remaining r = case r of
    Left rss -> map
      (foldl' (\remaining' -> concat . go remaining' . (rules Map.!)) remaining)
      rss
    Right c -> map
      (\case
        [] -> []
        (x : xs) | x == c    -> [xs]
                 | otherwise -> []
      )
      remaining

type Parser = Parsec Void Text

ruleP :: Parser (Int, Either [[Int]] Char)
ruleP =
  (,)
    <$> (L.decimal <* string ": ")
    <*> (   (Right <$> between (char '"') (char '"') letterChar)
        <|> (Left <$> (L.decimal `sepEndBy` char ' ') `sepBy` string "| ")
        )

inputP :: Parser (Map Int (Either [[Int]] Char), [[Char]])
inputP =
  (,)
    <$> (Map.fromList <$> ruleP `sepEndBy1` eol)
    <*> (eol *> some letterChar `sepEndBy1` eol)

parseFile :: MonadIO m => m (Map Int (Either [[Int]] Char), [[Char]])
parseFile =
  readFileUtf8 "./input/day19.txt"
    >>= either (throwString . errorBundlePretty) pure
    .   parse inputP "day19.txt"
