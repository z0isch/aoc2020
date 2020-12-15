module Day14 where

import           RIO                     hiding ( some
                                                , try
                                                , mask
                                                )

import           Text.Megaparsec

import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer
import           Data.Bits
import qualified RIO.Map                       as Map
import qualified RIO.List                      as List
import qualified RIO.Set                       as Set

part1 :: MonadIO f => f Integer
part1 =
  parseFile <&> sum . registers . \(i : is) -> foldl' step (initState i) is

part2 :: MonadIO f => f Integer
part2 =
  parseFile <&> sum . registers . \(i : is) -> foldl' step2 (initState i) is

data ProgState = ProgState { mask :: ([Int],Mask), registers :: Map Integer Integer}
    deriving (Eq, Show)

initState :: Inst -> ProgState
initState (MaskInst mask) = ProgState { mask, registers = mempty }
initState x               = error $ show x

step :: ProgState -> Inst -> ProgState
step p@ProgState {..} = \case
  MaskInst mask' -> p { mask = mask' }
  SetsInst Sets {..} ->
    p { registers = Map.insert register (applyMask value (snd mask)) registers }

step2 :: ProgState -> Inst -> ProgState
step2 p@ProgState {..} = \case
  MaskInst mask'     -> p { mask = mask' }
  SetsInst Sets {..} -> p
    { registers =
      let
        (floating, m) = mask
        startingReg   = register .|. bitsOn m
      in
        foldr
            ( (`Map.insert` value)
            . (\is -> foldl' clearBit
                             (foldl' setBit startingReg is)
                             (floating List.\\ is)
              )
            )
            registers
          $ possiblities floating
    }


applyMask :: Integer -> Mask -> Integer
applyMask value Mask {..} = (value .|. bitsOn) .&. bitsOff

possiblities :: Ord a => [a] -> [[a]]
possiblities xs =
  map Set.toList
    $ Set.toList
    $ Set.fromList
    $ fmap (Set.fromList . fst)
    $ concat
    $ List.scanr (=<<) [([], xs)]
    $ List.replicate (length xs) mkPossibility
 where
  mkPossibility (ys, zs) = do
    z <- zs
    pure (z : ys, zs List.\\ [z])

data Mask = Mask
    { bitsOn::Integer
    , bitsOff::Integer
    }
    deriving (Eq, Show)

data Sets = Sets {register::Integer, value::Integer}
    deriving (Eq, Show)

data Inst = MaskInst ([Int], Mask) | SetsInst Sets
    deriving (Eq, Show)

type Parser = Parsec Void Text

instP :: Parser Inst
instP = try maskP <|> setP

setP :: Parser Inst
setP =
  SetsInst
    <$> (   Sets
        <$> (string "mem" *> between (char '[') (char ']') decimal)
        <*> (string " = " *> decimal)
        )

maskP :: Parser Inst
maskP =
  MaskInst
    <$> (  string "mask = "
        *> (some alphaNumChar <&> \chars ->
             ( mkList (== 'X') chars
             , Mask { bitsOn  = foldl' mkOn 0 $ zip [0 ..] $ reverse chars
                    , bitsOff = foldl' mkOff 0 $ zip [0 ..] $ reverse chars
                    }
             )
           )
        )
 where
  mkList f = fmap fst . filter (f . snd) . zip [0 ..] . reverse
  mkOn a (i, c) =
    let f = case c of
          '1' -> setBit
          _   -> clearBit
    in  f a i
  mkOff a (i, c) =
    let f = case c of
          '0' -> clearBit
          _   -> setBit
    in  f a i

inputP :: Parser [Inst]
inputP = instP `sepBy1` eol

parseFile :: MonadIO m => m [Inst]
parseFile =
  readFileUtf8 "./input/day14.txt"
    >>= either (throwString . errorBundlePretty) pure
    .   parse inputP "day14.txt"
