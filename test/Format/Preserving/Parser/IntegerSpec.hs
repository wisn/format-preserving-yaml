{-# LANGUAGE OverloadedStrings #-}

module Parser.IntegerSpec where

import Test.Hspec

import Format.Preserving.YAML (parse)
import Format.Preserving.YAML.Parser.Token (Token (..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  context "with Int token" $ do
    it "returns single positive Int token" $ do
      parse "1234567890" `shouldBe` (Right [Int 1234567890])
    it "returns negative Int token between Space" $ do
      parse " -1234567890 "
      `shouldBe`
      (Right [Space, Int (-1234567890), Space])
    it "returns Str instead of Int" $ do
      parse "1234x0" `shouldBe` (Right [Str "1234x0"])
    it "returns Str instead of Int" $ do
      parse "12345     p" `shouldBe` (Right [Str "12345     p"])
  context "with Octal token" $ do
    it "returns single Octal token" $ do
      parse "0o01234567" `shouldBe` (Right [Octal "01234567"])
    it "returns Str token instead of Octal" $ do
      parse "0o012345678" `shouldBe` (Right [Str "0o012345678"])
  context "with Hexadecimal token" $ do
    it "returns single Hexadecimal content" $ do
      parse "0x0123456789ABCDEF"
      `shouldBe`
      (Right [Hexadecimal "0123456789ABCDEF"])
    it "returns Str token instead of Hexadecimal" $ do
      parse "0x0123456789ABCDEFG"
      `shouldBe`
      (Right [Str "0x0123456789ABCDEFG"])
