module Day7 where
import           RIO                     hiding ( try
                                                , some
                                                )

import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer
import qualified RIO.Map                       as Map
import qualified RIO.Set                       as Set
import           Data.Monoid

type BagRules = Map String (Map String Int)

part1 :: MonadIO f => f Int
part1 = length . bagSearch "shinygold" <$> parseFile

part2 :: MonadIO f => f (Sum Int)
part2 = bagsInBag "shinygold" <$> parseFile

bagSearch :: String -> BagRules -> Set String
bagSearch bag rules = go (bagsFitIn bag) mempty
 where
  bagsFitIn lookingFor =
    Map.keysSet $ Map.mapMaybe (Map.lookup lookingFor) rules
  go toSearch searched
    | null toSearch
    = searched
    | otherwise
    = let searched' = Set.union searched toSearch
          toSearch' =
              (`Set.difference` searched')
                $   Set.unions
                $   bagsFitIn
                <$> Set.toList toSearch
      in  go toSearch' searched'

bagsInBag :: String -> BagRules -> Sum Int
bagsInBag bag rules =
  maybe mempty
        (Map.foldMapWithKey (\b n -> Sum n <> ((n *) <$> bagsInBag b rules)))
    $ Map.lookup bag rules

type Parser = Parsec Void Text

bagP :: Parser String
bagP =
  (<>)
    <$> lexeme space1 (some letterChar)
    <*> lexeme space1 (some letterChar)
    <*  some letterChar

bagListP :: Parser [(Int, String)]
bagListP =
  ((,) <$> lexeme space1 decimal <*> bagP) `sepBy1` string ", " <* char '.'

ruleP :: Parser (String, [(Int, String)])
ruleP =
  (,)
    <$> lexeme space1 bagP
    <*> (  lexeme space1 (string "contain")
        *> (try bagListP <|> ([] <$ string "no other bags."))
        )

parseFile :: MonadIO m => m BagRules
parseFile =
  readFileUtf8 "./input/day7.txt"
    >>= either
          (throwString . errorBundlePretty)
          (pure . Map.fromList . fmap
            (\(b, bs) -> (b, Map.fromList $ fmap (\(x, y) -> (y, x)) bs))
          )
    .   parse (ruleP `sepBy1` eol) "day7.txt"
