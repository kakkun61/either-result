module Data.Either.ResultSpec (spec) where

import Data.Either.Result

import Test.Hspec

import Data.Coerce

spec :: Spec
spec = do
  describe "coercible" $ do
    it "Result Int -> Either String Int" $ do
      let
        actual = coerce $ Success (0 :: Int)
        expected = Right 0 :: Either String Int
      actual `shouldBe` expected
