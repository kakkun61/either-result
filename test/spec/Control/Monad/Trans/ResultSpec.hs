module Control.Monad.Trans.ResultSpec (spec) where

import Control.Monad.Trans.Result

import Test.Hspec

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

    it "result id id (Error e) == e" $ do
      let e = "e"
      result id id (Error e) `shouldBe` e

    it "result id id (Success a) == a" $ do
      let a = "a"
      result id id (Success a) `shouldBe` a

    it "fromSuccess b (Error e) == b" $ do
      let b = "b"
      fromSuccess b (Error "e") `shouldBe` b

    it "fromSuccess b (Success a) == a" $ do
      let a = "a"
      fromSuccess "b" (Success a) `shouldBe` a

    it "show (Error \"e\") == \"Error \\\"e\\\"\"" $ do
      show (Error "e" :: Result Int) `shouldBe` "Error \"e\""

    it "show (Success 1) == \"Success 1\"" $ do
      show (Success (1 :: Int)) `shouldBe` "Success 1"

    it "read \"Error \\\"e\\\"\" == Error \"e\"" $ do
      read "Error \"e\"" `shouldBe` (Error "e" :: Result Int)

    it "read \"Success 1\" == Success 1" $ do
      read "Success 1" `shouldBe` Success (1 :: Int)

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
