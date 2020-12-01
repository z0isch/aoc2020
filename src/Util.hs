{-# LANGUAGE OverloadedLabels #-}

module Util where

import RIO

import Data.Generics.Labels ()
import Control.Lens ((-=), (+=))
import RIO.State
plus2 :: Int -> Int
plus2 = (+ 2)

data Foo = Foo { bar :: Bar}
  deriving (Show, Eq, Generic)
data Bar = Bar {a :: Int}
  deriving (Show, Eq, Generic)

doNothing :: Foo -> Foo
doNothing = execState ((#bar . #a) += 1 >> (#bar . #a) -= 1)
