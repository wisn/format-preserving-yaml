{-# LANGUAGE OverloadedStrings #-}

module Formatter.CommentSpec where

import Test.Hspec

import Format.Preserving.YAML (format)
import Format.Preserving.YAML.Parser.Token (Token (..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  context "with Comment token" $ do
    it "returns single Comment content" $ do
      format [Comment " YAML 1.2"] `shouldBe` "# YAML 1.2"
    it "returns two Comment content with Tab in it" $ do
      format [Comment " Hello", LineFeed, Comment " YAML\t1.2"]
      `shouldBe`
      "# Hello\n# YAML\t1.2"
