module Day4 where

import           RIO                     hiding ( try
                                                , some
                                                )

import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer
import qualified RIO.Map                       as Map
import qualified RIO.Text                      as Text
import qualified RIO.Set                       as Set

part1 :: MonadIO f => f Int
part1 =
  length
    .   filter ((== length requiredFields) . length)
    .   fmap (Set.intersection requiredFields . Map.keysSet)
    <$> parseFile

part2 :: MonadIO f => f Int
part2 = length . mapMaybe mkPassport <$> parseFile

type Parser = Parsec Void Text

textP :: Parser Text
textP = Text.pack <$> some (alphaNumChar <|> char '#')

fieldP :: Parser (Text, Text)
fieldP = (,) <$> textP <* char ':' <*> textP

passportLineP :: Parser [(Text, Text)]
passportLineP = fieldP `sepBy1` char ' '

passportP :: Parser (Map Text Text)
passportP = Map.fromList . concat <$> passportLineP `sepEndBy1` eol

parseFile :: MonadIO m => m [Map Text Text]
parseFile =
  readFileUtf8 "./input/day4.txt"
    >>= either (throwString . errorBundlePretty) pure
    .   parse (passportP `sepBy1` eol) "day4.txt"

requiredFields :: Set Text
requiredFields = Set.fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

data Height = Cm Integer | In Integer
    deriving stock (Eq,Show)

data Passport = Passport
    { byr :: Integer
    , iyr :: Integer
    , eyr :: Integer
    , hgt :: Height
    , hcl :: Text
    , ecl :: Text
    , pid :: Text
    }
    deriving stock (Eq,Show)

dateBetweenM :: Integer -> Integer -> Integer -> Maybe Integer
dateBetweenM x y d | d >= x && d <= y = Just d
                   | otherwise        = Nothing

heightP :: Parser Height
heightP = do
  n <- decimal
  Cm n <$ string "cm" <|> In n <$ string "in"

heightM :: Height -> Maybe Height
heightM (Cm cm) | cm >= 150 && cm <= 193 = Just (Cm cm)
                | otherwise              = Nothing
heightM (In inches) | inches >= 59 && inches <= 76 = Just (In inches)
                    | otherwise                    = Nothing


hairColorP :: Parser Text
hairColorP = Text.pack <$> (char '#' *> some alphaNumChar)

hairColorM :: Text -> Maybe Text
hairColorM c | Text.length c == 6 = Just c
             | otherwise          = Nothing

eyeColorP :: Parser Text
eyeColorP =
  asum (fmap (try . string) ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])

pidM :: Text -> Maybe Text
pidM p | Text.length p == 9 = Just p
       | otherwise          = Nothing

mkPassport :: Map Text Text -> Maybe Passport
mkPassport fields = do
  byr <-
    dateBetweenM 1920 2002
    =<< parseMaybeText decimal
    =<< Map.lookup "byr" fields
  iyr <-
    dateBetweenM 2010 2020
    =<< parseMaybeText decimal
    =<< Map.lookup "iyr" fields
  eyr <-
    dateBetweenM 2020 2030
    =<< parseMaybeText decimal
    =<< Map.lookup "eyr" fields
  hgt <- heightM =<< parseMaybeText heightP =<< Map.lookup "hgt" fields
  hcl <- hairColorM =<< parseMaybeText hairColorP =<< Map.lookup "hcl" fields
  ecl <- parseMaybeText eyeColorP =<< Map.lookup "ecl" fields
  pid <- pidM =<< Map.lookup "pid" fields
  pure Passport { .. }
 where
  parseMaybeText :: Parser a -> Text -> Maybe a
  parseMaybeText = parseMaybe
