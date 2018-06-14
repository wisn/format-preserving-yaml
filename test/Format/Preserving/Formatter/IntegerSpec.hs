{-# LANGUAGE OverloadedStrings #-}

module Formatter.IntegerSpec where

import Test.Hspec

import Format.Preserving.YAML (format)
import Format.Preserving.YAML.Parser.Token (Token (..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  context "with Int token" $ do
    it "returns single positive Int content" $ do
      format [Int 1234567890] `shouldBe` "1234567890"
    it "returns negative Int content between Space" $ do
      format [Space, Int (-1234567890), Space]
      `shouldBe`
      " -1234567890 "
  context "with Octal token" $ do
    it "returns single Octal content" $ do
      format [Octal "01234567"] `shouldBe` "0o01234567"
  context "with Hexadecimal token" $ do
    it "returns single Hexadecimal content" $ do
      format [Hexadecimal "0123456789ABCDEF"]
      `shouldBe`
      "0x0123456789ABCDEF"
