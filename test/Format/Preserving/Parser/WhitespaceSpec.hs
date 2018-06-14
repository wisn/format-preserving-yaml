{-# LANGUAGE OverloadedStrings #-}

module Parser.WhitespaceSpec where

import Test.Hspec

import Format.Preserving.YAML (parse)
import Format.Preserving.YAML.Parser.Token (Token (..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  context "with Space token" $ do
    it "returns single Space token" $ do
      parse " " `shouldBe` (Right [Space])
    it "returns several Space tokens" $ do
      parse "   " `shouldBe` (Right [Space, Space, Space])
    it "returns Space tokens between Scalar" $ do
      parse " 1 " `shouldBe` (Right [Space, Int 1, Space])
  context "with Tab token" $ do
    it "returns single Tab token" $ do
      parse "\t" `shouldBe` (Right [Tab])
    it "returns several Tab tokens" $ do
      parse "\t\t\t" `shouldBe` (Right [Tab, Tab, Tab])
    it "return Tab tokens between Scalar" $ do
      parse "\tnull\t" `shouldBe` (Right [Tab, Null "null", Tab])
