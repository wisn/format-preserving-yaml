{-# LANGUAGE OverloadedStrings #-}

module Parser.FloatingSpec where

import Test.Hspec

import Format.Preserving.YAML (parse)
import Format.Preserving.YAML.Parser.Token (Token (..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  context "with Inf token" $ do
    it "returns single Inf (inf) token" $ do
      parse ".inf" `shouldBe` (Right [Inf ".inf"])
    it "returns single Inf (-Inf) token" $ do
      parse "-.Inf" `shouldBe` (Right [Inf "-.Inf"])
    it "returns single Inf (INF) token" $ do
      parse ".INF" `shouldBe` (Right [Inf ".INF"])
    it "returns single Inf (+INF) token" $ do
      parse "+.INF" `shouldBe` (Right [Inf "+.INF"])
  context "with Nan token" $ do
    it "returns single Nan (nan) token" $ do
      parse ".nan" `shouldBe` (Right [Nan "nan"])
    it "returns single Nan (NaN) token" $ do
      parse ".NaN" `shouldBe` (Right [Nan "NaN"])
    it "returns single Nan (NAN) token" $ do
      parse ".NAN" `shouldBe` (Right [Nan "NAN"])
