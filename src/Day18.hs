module Day18 where

import           RIO                     hiding ( (<|>) )

import qualified RIO.Text                      as T
import           Text.Parsec.Expr
import           Text.Parsec
import qualified Text.Parsec.Token             as P
import           Text.Parsec.Language           ( haskell )

part1 :: MonadIO f => f (Either ParseError Integer)
part1 = fmap sum . parse (some (expr table)) "" . T.unpack <$> readFileUtf8
  "./input/day18.txt"

part2 :: MonadIO f => f (Either ParseError Integer)
part2 = fmap sum . parse (some (expr table2)) "" . T.unpack <$> readFileUtf8
  "./input/day18.txt"

table = [[binary "*" (*) AssocLeft, binary "+" (+) AssocLeft]]

table2 = [[binary "+" (+) AssocLeft], [binary "*" (*) AssocLeft]]

binary name fun = Infix $ fun <$ P.reservedOp haskell name

term e = P.parens haskell e <|> P.natural haskell

expr t = buildExpressionParser t (term $ expr t)
