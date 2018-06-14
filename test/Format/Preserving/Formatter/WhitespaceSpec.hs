{-# LANGUAGE OverloadedStrings #-}

module Formatter.WhitespaceSpec where

import Test.Hspec

import Format.Preserving.YAML (format)
import Format.Preserving.YAML.Parser.Token (Token (..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  context "with Space token" $ do
    it "returns single Space content" $ do
      format [Space] `shouldBe` " "
    it "returns several Space content" $ do
      format [Space, Space, Space] `shouldBe` "   "
    it "returns Space content between Scalar" $ do
      format [Space, Int 1, Space] `shouldBe` " 1 "
  context "with Tab token" $ do
    it "returns single Tab content" $ do
      format [Tab] `shouldBe` "\t"
    it "returns several Tab content" $ do
      format [Tab, Tab, Tab] `shouldBe` "\t\t\t"
    it "return Tab tokens between Scalar" $ do
      format [Tab, Null "null", Tab] `shouldBe` "\tnull\t"
