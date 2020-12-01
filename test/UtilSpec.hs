module UtilSpec
  ( spec
  )
where

import Import
import Util
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "FOO" $ do
    it "lens" $ doNothing (Foo (Bar 1)) `shouldBe` Foo (Bar 1)
  describe "plus2" $ do
    it "basic check" $ plus2 0 `shouldBe` 2
    it "overflow" $ plus2 maxBound `shouldBe` minBound + 1
    prop "minus 2" $ \i -> plus2 i - 2 `shouldBe` i
