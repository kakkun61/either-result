module Control.Monad.Trans.Except.ResultSpec (spec) where

import Control.Monad.Trans.Except.Result

import Test.Hspec

import Control.Monad.Trans.Except (ExceptT (ExceptT))
import Data.Coerce                (coerce)
import Data.Functor.Identity      (Identity (Identity))

spec :: Spec
spec = do
  describe "monad" $ do
    it "runResult . Result == id" $ do
      let e = pure 'a'
      runResult (Result e) `shouldBe` e

    it "Result . runResult == id" $ do
      let r = pure 'a'
      Result (runResult r) `shouldBe` r

    it "b == a where Result b = Result a" $ do
      let
        a = pure 'a'
        Result b = Result a
      b `shouldBe` a

    describe "coercible" $ do
      it "Result Int -> Either String Int" $ do
        let
          actual = coerce $ Result $ pure (0 :: Int)
          expected = Right 0 :: Either String Int
        actual `shouldBe` expected

  describe "monad transformer" $ do
    it "runResultT . ResultT == id" $ do
      let e = pure 'a'
      runResult (Result e) `shouldBe` e

    it "Result . runResult == id" $ do
      let r = pure 'a'
      Result (runResult r) `shouldBe` r

  describe "exception" $ do
    it "throwE e `catchE` pure == pure e" $ do
      let
        e = "err"
        m = throwE e `catchE` pure :: Result String
      m `shouldBe` pure e

    it "pure a `catchE` pure == pure a" $ do
      let
        a = "ok"
        m = pure a `catchE` pure :: Result String
      m `shouldBe` pure a
