{-# LANGUAGE StrictData #-}

module Day16 where

import           RIO                     hiding ( some
                                                , try
                                                )

import           Control.Monad.Loops            ( iterateUntilM )
import           Text.Megaparsec

import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer
import qualified RIO.List.Partial              as List
import qualified RIO.List                      as List
import qualified RIO.Map                       as Map
import qualified RIO.Set                       as Set
import qualified RIO.Text                      as T

part1 :: MonadIO f => f Int
part1 = sum . invalidTicketPieces <$> parseFile

part2 :: MonadIO m => m Int
part2 = do
  vs <- validTickets <$> parseFile
  let
    fieldLookup     = ticketsPossible $ List.head $ perms $ initTicketPerm vs
    departureFields = Set.filter (T.isPrefixOf "departure" . fst) fieldLookup
    vals =
      mapMaybe ((`Map.lookup` myTicket vs) . snd) $ Set.toList departureFields
  pure $ product vals

data TicketPerm = TicketPerm
  { rulesLeft :: Map Text [(Int, Int)]
  , ticketsLeft :: [Map Int Int]
  , ticketsPossible :: Set (Text, Int)
  }
  deriving (Eq,Show, Ord)

perms :: TicketPerm -> [TicketPerm]
perms = iterateUntilM (Map.null . rulesLeft) permStep

initTicketPerm :: TicketInput -> TicketPerm
initTicketPerm TicketInput {..} = TicketPerm { rulesLeft = rules
                                             , ticketsLeft = myTicket : tickets
                                             , ticketsPossible = mempty
                                             }

permStep :: TicketPerm -> [TicketPerm]
permStep TicketPerm {..} =
  let (rName, validFields) =
          List.head
            $   List.sortOn (length . snd)
            $   Map.toList
            $   rulesLeft
            <&> \r -> filter (all ((`inRange` r) . snd)) $ List.transpose $ map
                  Map.toList
                  ticketsLeft
  in  validFields <&> \((field, _) : _) -> TicketPerm
        { rulesLeft       = Map.delete rName rulesLeft
        , ticketsLeft     = map (Map.delete field) ticketsLeft
        , ticketsPossible = Set.insert (rName, field) ticketsPossible
        }

validTickets :: TicketInput -> TicketInput
validTickets t =
  t { tickets = filter (all (\i -> any (inRange i) $ rules t)) $ tickets t }

invalidTicketPieces :: TicketInput -> [Int]
invalidTicketPieces TicketInput { rules, tickets } =
  concatMap (filter (\i -> not $ any (inRange i) rules) . Map.elems) tickets

inRange :: Int -> [(Int, Int)] -> Bool
inRange i = any (\(x, y) -> i >= x && i <= y)

data TicketInput = TicketInput
    { rules :: Map Text [(Int, Int)]
    , myTicket :: Map Int Int
    , tickets :: [Map Int Int]
    }
    deriving (Eq,Show)

type Parser = Parsec Void Text

ticketP :: Parser (Map Int Int)
ticketP = Map.fromList . zip [0 ..] <$> decimal `sepBy1` char ','

rangeP :: Parser (Int, Int)
rangeP = (,) <$> decimal <*> (char '-' *> decimal)

rangesP :: Parser [(Int, Int)]
rangesP = (:) <$> rangeP <*> (string " or " *> (pure <$> rangeP))

ruleP :: Parser (Text, [(Int, Int)])
ruleP = (,) <$> takeWhile1P Nothing (/= ':') <*> (string ": " *> rangesP)

myTicketP :: Parser (Map Int Int)
myTicketP = string "your ticket:" *> eol *> ticketP

ticketsP :: Parser [Map Int Int]
ticketsP = string "nearby tickets:" *> eol *> ticketP `sepBy1` eol

inputP :: Parser TicketInput
inputP =
  TicketInput
    <$> (Map.fromList <$> try ruleP `sepEndBy1` eol <* eol)
    <*> (myTicketP <* eol <* eol)
    <*> ticketsP

parseFile :: MonadIO m => m TicketInput
parseFile =
  readFileUtf8 "./input/day16.txt"
    >>= either (throwString . errorBundlePretty) pure
    .   parse inputP "day16.txt"
