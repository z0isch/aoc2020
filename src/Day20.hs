{-# LANGUAGE TupleSections #-}
module Day20 where

import           RIO                     hiding ( some )

import qualified RIO.Map                       as Map
import qualified RIO.Map.Partial               as Map
import qualified RIO.List                      as List
import qualified RIO.List.Partial              as List
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

import           Linear.V2
import           Linear.Epsilon
import           Linear.Matrix
import           Control.Monad.Loops            ( iterateUntilM )

part1 = product . gridEdgesShared <$> parseFile

type Grid = Map (V2 Double) Bool

data Puzzle = Puzzle { toPlace :: Map Int Grid, placed :: Map (V2 Double) (Maybe Grid) }
  deriving (Eq, Show)

foo = iterateUntilM (null . toPlace) puzzleStep

initPuzzle :: Map Int Grid -> Puzzle
initPuzzle toPlace = Puzzle { toPlace, placed }
 where
  placed = Map.fromList $ map (, Nothing) $ concatMap
    (\x -> map (V2 x) [0 .. 9])
    [0 .. 9]

puzzleStep :: Puzzle -> [Puzzle]
puzzleStep p@Puzzle { toPlace, placed } = do
  (nextPlace, (gridNum, grid)) <- possibles p
  pure $ Puzzle { toPlace = Map.delete gridNum toPlace
                , placed  = Map.insert nextPlace (Just grid) placed
                }

possibles :: Puzzle -> [(V2 Double, (Int, Grid))]
possibles Puzzle { toPlace, placed } =
  map (nextPlace, ) $ concat $ Map.elems $ Map.mapWithKey
    (\gridNum grid ->
      map (gridNum, )
        $ filter
            (\g1 -> all (\(d, g2) -> shareSide g2 g1 d)
              $ orthoginal placed nextPlace
            )
        $ allRots grid
    )
    toPlace
  where (nextPlace, _) = Map.findMin $ Map.filter isNothing placed

data Dir = U | D | L | R

shareSide :: Grid -> Grid -> Dir -> Bool
shareSide g1 g2 = \case
  U -> upSide g1 == downSide g2
  D -> downSide g1 == upSide g2
  L -> leftSide g1 == rightSide g2
  R -> rightSide g1 == leftSide g2
 where
  upSide    = List.head . allCoords
  downSide  = List.last . allCoords
  leftSide  = List.head . List.transpose . allCoords
  rightSide = List.last . List.transpose . allCoords
  allCoords g = Map.toAscList g

orthoginal :: Map (V2 Double) (Maybe Grid) -> V2 Double -> [(Dir, Grid)]
orthoginal p (V2 x y) = mapMaybe
  (\(d, c) -> (d, ) <$> join (Map.lookup c p))
  [(R, V2 (x + 1) y), (L, V2 (x - 1) y), (U, V2 x (y + 1)), (D, V2 x (y - 1))]

corners :: Ord k => Map k Grid -> [k]
corners = Map.keys . Map.filter (== 2) . gridEdgesShared

gridEdgesShared :: Ord k => Map k Grid -> Map k Int
gridEdgesShared grids = Map.mapWithKey
  (\i grid -> Map.size $ Map.filter (shareAnEdge grid) $ Map.delete i grids)
  grids

edgeVs :: (Num a, Enum a) => [[V2 a]]
edgeVs =
  [ List.head $ List.transpose allCoords
  , List.last $ List.transpose allCoords
  , List.head allCoords
  , List.last allCoords
  ]
  where allCoords = map (\x -> map (V2 x) [0 .. 9]) [0 .. 9]

edges :: Grid -> [[Bool]]
edges g = map (map (g Map.!)) edgeVs

shareAnEdge :: Grid -> Grid -> Bool
shareAnEdge g1 g2 =
  any (\e -> e `elem` edges g2 || reverse e `elem` edges g2) $ edges g1

allRots :: Grid -> [Grid]
allRots g = map (\r -> Map.mapKeysWith const (`rot` r) g) [0, 90, 180, 270]

mkZero :: Epsilon p => p -> p
mkZero x | nearZero x = 0
         | otherwise  = x

rot :: V2 Double -> Double -> V2 Double
rot v deg =
  let rad    = deg * pi / 180
      rotMat = fmap mkZero
        <$> V2 (V2 (cos rad) (-(sin rad))) (V2 (sin rad) (cos rad))
  in  v *! rotMat

type Parser = Parsec Void Text

gridP :: Parser Grid
gridP =
  Map.fromList
    . (concat . zipWith (\y -> zipWith (\x -> (V2 x y, )) [0 ..]) [0 ..])
    <$> some (True <$ char '#' <|> False <$ char '.')
    `sepEndBy1` eol

tileP :: Parser (Int, Grid)
tileP = (,) <$> (string "Tile " *> L.decimal <* char ':' <* eol) <*> gridP

inputP :: Parser (Map Int Grid)
inputP = Map.fromList <$> tileP `sepEndBy1` eol

parseFile :: MonadIO m => m (Map Int Grid)
parseFile =
  readFileUtf8 "./input/day20.txt"
    >>= either (throwString . errorBundlePretty) pure
    .   parse inputP "day20.txt"
