{-# LANGUAGE TupleSections #-}
module Day21 where

import           RIO                     hiding ( try
                                                , some
                                                )

import qualified RIO.HashMap                   as HashMap
import qualified RIO.HashSet                   as HashSet
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Data.Semigroup
import qualified RIO.List                      as List
import qualified RIO.List.Partial              as List
import           Data.Tuple                     ( swap )

part1 :: [([String], [String])] -> Maybe (Sum Int)
part1 rs = foldMap
  ( fmap (Sum . HashSet.size . HashSet.intersection noAllergens)
  . listToMaybe
  . HashMap.elems
  )
  rules
 where
  noAllergens     = allIngredients `HashSet.difference` fold ingredientsLeft
  allIngredients  = HashSet.fromList $ concatMap fst rs
  ingredientsLeft = possibleIngredients rules
  rules           = mkRules rs

part2 :: [([String], [String])] -> [Char]
part2 rs = List.intercalate "," $ map fst $ List.sortOn snd $ HashMap.toList
  allergens
 where
  allergens =
    foo
        (HashMap.singleton (List.head $ HashSet.toList foundIngredient)
                           foundAlergen
        )
      $ HashMap.fromList rest
  (foundAlergen, foundIngredient) : rest =
    List.sortOn (length . snd) $ HashMap.toList $ possibleIngredients $ mkRules
      rs

foo
  :: HashMap String String
  -> HashMap String (HashSet String)
  -> HashMap String String
foo allergens rules | HashMap.null rules = allergens
                    | otherwise          = foo allergens' rules'
 where
  allergens' = HashMap.union allergens newAllergens
  newAllergens =
    HashMap.fromList $ map swap $ HashMap.toList $ HashMap.mapMaybe
      (\is ->
        if length is == 1 then listToMaybe $ HashSet.toList is else Nothing
      )
      runRules
  rules'   = HashMap.filter ((> 1) . length) runRules
  runRules = fmap
    (\ingredients ->
      HashSet.difference ingredients $ HashSet.fromList $ HashMap.keys allergens
    )
    rules

mkRules :: [([String], [String])] -> [HashMap String (HashSet String)]
mkRules = map
  (\(ingredients, rules) ->
    HashMap.fromList $ map (, HashSet.fromList ingredients) rules
  )

possibleIngredients
  :: [HashMap String (HashSet String)] -> HashMap String (HashSet String)
possibleIngredients rules = HashMap.fromList $ foldMap
  (\allergen ->
    [ ( allergen
      , foldr HashSet.intersection allIngredients
        $ mapMaybe (HashMap.lookup allergen) rules
      )
    ]
  )
  allAllergens
 where
  allIngredients = foldMap fold rules
  allAllergens   = HashSet.fromList $ concatMap HashMap.keys rules


type Parser = Parsec Void Text

allergenP :: Parser [String]
allergenP =
  between (char '(') (char ')')
    $       string "contains "
    *>      some letterChar
    `sepBy` string ", "

inputP :: Parser [([String], [String])]
inputP =
  ((,) <$> some (try letterChar) `sepEndBy` space <*> allergenP) `sepBy1` eol

parseFile :: MonadIO m => m [([String], [String])]
parseFile =
  readFileUtf8 "./input/day21.txt"
    >>= either (throwString . errorBundlePretty) pure
    .   parse inputP "day21.txt"

test :: [([String], [String])]
Just test = parseMaybe
  inputP
  "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)\ntrh fvjkl sbzzf mxmxvkd (contains dairy)\nsqjhc fvjkl (contains soy)\nsqjhc mxmxvkd sbzzf (contains fish)"
