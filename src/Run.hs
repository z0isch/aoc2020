module Run
  ( run
  )
where

import           Import

import           Day15

run :: RIO App ()
run = do
  logInfo "We're inside the application!"
  hPutBuilder stdout . fromString . (<> "\n") . show $ part2
