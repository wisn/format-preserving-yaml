{-# LANGUAGE OverloadedStrings #-}

module Parser.NewlineSpec where

import Test.Hspec

import Format.Preserving.YAML (parse)
import Format.Preserving.YAML.Parser.Token (Token (..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  context "with LineFeed token" $ do
    it "returns single LineFeed token" $ do
      parse "\n" `shouldBe` (Right [LineFeed])
    it "returns several LineFeed tokens" $ do
      parse "\n\n\n" `shouldBe` (Right [LineFeed, LineFeed, LineFeed])
    it "returns LineFeed tokens between Whitespace" $ do
      parse "\n \n" `shouldBe` (Right [LineFeed, Space, LineFeed])
    it "returns LineFeed tokens between Scalar" $ do
      parse "\n1234\n" `shouldBe` (Right [LineFeed, Int 1234, LineFeed])
  context "with CarriageReturn token" $ do
    it "returns single CarriageReturn token" $ do
      parse "\r" `shouldBe` (Right [CarriageReturn])
    it "returns several CarriageReturn tokens" $ do
      parse "\r\r" `shouldBe` (Right [CarriageReturn, CarriageReturn])
    it "returns CarriageReturn and LineFeed tokens" $ do
      parse "\r\n" `shouldBe` (Right [CarriageReturn, LineFeed])
    it "returns CarriageReturn tokens between Scalar" $ do
      parse "\r-5\r"
      `shouldBe`
      (Right [CarriageReturn, Int (-5), CarriageReturn])
