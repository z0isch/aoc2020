module Day8 where

import           RIO                     hiding ( try )

import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer
import qualified RIO.Map                       as Map
import qualified RIO.Map.Partial               as Map
import qualified RIO.Set                       as Set
import qualified RIO.List                      as List

type LineNum = Integer

part1 :: MonadIO f => f (Maybe Integer)
part1 = fmap acc . runProg . initialProgState . toInstMap <$> parseFile

part2 :: MonadIO f => f (Maybe Integer)
part2 =
  fmap acc
    .   List.headMaybe
    .   dropWhile (not . progEnd)
    .   mapMaybe (runProg . initialProgState)
    .   (\i -> flipToNop i <> flipToJmp i)
    .   toInstMap
    <$> parseFile

getInt :: Inst -> Integer
getInt = \case
  Jmp i -> i
  Acc i -> i
  Nop i -> i

flipToJmp :: Map LineNum Inst -> [Map LineNum Inst]
flipToJmp = flipTo isNop Jmp
 where
  isNop = \case
    Nop _ -> True
    _     -> False

flipToNop :: Map LineNum Inst -> [Map LineNum Inst]
flipToNop = flipTo isJmp Nop
 where
  isJmp = \case
    Jmp _ -> True
    _     -> False

flipTo
  :: (Inst -> Bool)
  -> (Integer -> Inst)
  -> Map LineNum Inst
  -> [Map LineNum Inst]
flipTo isInst toInst m =
  mapMaybe (\lineNum -> foo lineNum =<< Map.lookup lineNum m) $ Map.keys m
 where
  foo lineNum i = if isInst i
    then Just $ Map.insert lineNum (toInst $ getInt i) m
    else Nothing

runProg :: ProgState -> Maybe ProgState
runProg =
  List.headMaybe
    . List.dropWhile (\p -> not (loopDetected p) && not (progEnd p))
    . List.iterate doInst

data Inst = Acc Integer | Jmp Integer | Nop Integer
    deriving (Eq, Show)

data ProgState = ProgState
    { seen :: Set LineNum
    , curr :: LineNum
    , instructions :: Map LineNum Inst
    , acc :: Integer
    , progEnd :: Bool
    }
    deriving (Eq, Show)

initialProgState :: Map LineNum Inst -> ProgState
initialProgState instructions =
  ProgState { seen = mempty, curr = 0, instructions, acc = 0, progEnd = False }

toInstMap :: (Ord k, Num k, Enum k) => [a] -> Map k a
toInstMap inst = Map.fromList $ (\(x, y) -> (y, x)) <$> zip inst [0 ..]

loopDetected :: ProgState -> Bool
loopDetected ProgState {..} = curr `Set.member` seen

doInst :: ProgState -> ProgState
doInst p@ProgState {..} =
  if progEnd || curr == fromIntegral (length instructions)
    then p { progEnd = True }
    else
      let p' = p { seen = Set.insert curr seen }
          i  = instructions Map.! curr
      in  case i of
            Acc a -> p' { curr = curr + 1, acc = acc + a }
            Jmp j -> p' { curr = curr + j }
            Nop _ -> p' { curr = curr + 1 }

type Parser = Parsec Void Text

instPieceP :: Text -> Parser Integer
instPieceP s =
  lexeme space1 (string s) *> signed Text.Megaparsec.Char.space decimal

instP :: Parser Inst
instP = asum $ map
  try
  [Acc <$> instPieceP "acc", Jmp <$> instPieceP "jmp", Nop <$> instPieceP "nop"]

parseFile :: MonadIO m => m [Inst]
parseFile =
  readFileUtf8 "./input/day8.txt"
    >>= either (throwString . errorBundlePretty) pure
    .   parse (instP `sepBy1` eol) "day8.txt"
