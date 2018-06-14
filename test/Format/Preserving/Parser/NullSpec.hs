{-# LANGUAGE OverloadedStrings #-}

module Parser.NullSpec where

import Test.Hspec

import Format.Preserving.YAML (parse)
import Format.Preserving.YAML.Parser.Token (Token (..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  context "with Null token" $ do
    it "returns single Null (null) token" $ do
      parse "null" `shouldBe` (Right [Null "null"])
    it "returns single Null (Null) token" $ do
      parse "Null" `shouldBe` (Right [Null "Null"])
    it "returns single Null (NULL) token" $ do
      parse "NULL " `shouldBe` (Right [Null "NULL", Space])
    it "returns single Null (~) token" $ do
      parse "~" `shouldBe` (Right [Null "~"])
