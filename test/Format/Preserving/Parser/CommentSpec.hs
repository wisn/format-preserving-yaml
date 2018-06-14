{-# LANGUAGE OverloadedStrings #-}

module Parser.CommentSpec where

import Test.Hspec

import Format.Preserving.YAML (parse)
import Format.Preserving.YAML.Parser.Token (Token (..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  context "with Comment token" $ do
    it "returns single Comment token" $ do
      parse "# YAML 1.2" `shouldBe` (Right [Comment " YAML 1.2"])
    it "returns two Comment tokens with Tab in it" $ do
      parse "# Hello\n# YAML\t1.2"
      `shouldBe`
      (Right [Comment " Hello", LineFeed, Comment " YAML\t1.2"])
