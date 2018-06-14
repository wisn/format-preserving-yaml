{-# LANGUAGE OverloadedStrings #-}

module Formatter.FloatingSpec where

import Test.Hspec

import Format.Preserving.YAML (format)
import Format.Preserving.YAML.Parser.Token (Token (..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  context "with Inf token" $ do
    it "returns single Inf (inf) content" $ do
      format [Inf ".inf"] `shouldBe` ".inf"
    it "returns single Inf (-Inf) content" $ do
      format [Inf "-.Inf"] `shouldBe` "-.Inf"
    it "returns single Inf (INF) content" $ do
      format [Inf ".INF"] `shouldBe` ".INF"
    it "returns single Inf (+INF) content" $ do
      format [Inf "+.INF"] `shouldBe` "+.INF"
  context "with Nan token" $ do
    it "returns single Nan (nan) content" $ do
      format [Nan "nan"] `shouldBe` ".nan"
    it "returns single Nan (NaN) content" $ do
      format [Nan "NaN"] `shouldBe` ".NaN"
    it "returns single Nan (NAN) content" $ do
      format [Nan "NAN"] `shouldBe` ".NAN"
