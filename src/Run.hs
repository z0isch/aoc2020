module Run
  ( run
  )
where

import           Import

import           Day16
import qualified RIO.Text                      as T

run :: RIO App ()
run = do
  logInfo "We're inside the application!"
  hPutBuilder stdout . fromString . T.unpack . (<> "\n") . tshow =<< part2
