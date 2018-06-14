{-# LANGUAGE OverloadedStrings #-}

module Formatter.NullSpec where

import Test.Hspec

import Format.Preserving.YAML (format)
import Format.Preserving.YAML.Parser.Token (Token (..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  context "with Null token" $ do
    it "returns single Null (null) content" $ do
      format [Null "null"] `shouldBe` "null"
    it "returns single Null (Null) content" $ do
      format [Null "Null"] `shouldBe` "Null"
    it "returns single Null (NULL) content" $ do
      format [Null "NULL", Space] `shouldBe` "NULL "
    it "returns single Null (~) content" $ do
      format [Null "~"] `shouldBe` "~"
