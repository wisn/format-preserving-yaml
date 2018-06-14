{-# LANGUAGE OverloadedStrings #-}

module Formatter.NewlineSpec where

import Test.Hspec

import Format.Preserving.YAML (format)
import Format.Preserving.YAML.Parser.Token (Token (..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  context "with LineFeed token" $ do
    it "returns single LineFeed content" $ do
      format [LineFeed] `shouldBe` "\n"
    it "returns several LineFeed content" $ do
      format [LineFeed, LineFeed, LineFeed] `shouldBe` "\n\n\n"
    it "returns LineFeed content between Whitespace" $ do
      format [LineFeed, Space, LineFeed] `shouldBe` "\n \n"
    it "returns LineFeed content between Scalar" $ do
      format [LineFeed, Int 1234, LineFeed] `shouldBe` "\n1234\n"
  context "with CarriageReturn token" $ do
    it "returns single CarriageReturn content" $ do
      format [CarriageReturn] `shouldBe` "\r"
    it "returns several CarriageReturn content" $ do
      format [CarriageReturn, CarriageReturn] `shouldBe` "\r\r"
    it "returns CarriageReturn and LineFeed content" $ do
      format  [CarriageReturn, LineFeed] `shouldBe` "\r\n"
    it "returns CarriageReturn content between Scalar" $ do
      format [CarriageReturn, Int (-5), CarriageReturn]
      `shouldBe`
      "\r-5\r"
