{-# LANGUAGE OverloadedStrings #-}

module Formatter.BooleanSpec where

import Test.Hspec

import Format.Preserving.YAML (format)
import Format.Preserving.YAML.Parser.Token (Token (..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  context "with True token" $ do
    it "returns single Bool (true) content" $ do
      format [Bool True "true"] `shouldBe` "true"
    it "returns Bool (True) content between Space" $ do
      format [Space, Bool True "True", Space] `shouldBe` " True "
    it "returns Bool (TRUE) content between Newline" $ do
      format [LineFeed, Tab, Bool True "TRUE", LineFeed]
      `shouldBe`
      "\n\tTRUE\n"
  context "with False token" $ do
    it "returns single Bool (false) content" $ do
      format [Bool False "false"] `shouldBe` "false"
    it "returns Bool (False) content between Space" $ do
      format [Space, Bool False "False", Space] `shouldBe` " False "
    it "returns Bool (FALSE) content between Newline" $ do
      format [CarriageReturn, Bool False "FALSE", CarriageReturn]
      `shouldBe`
      "\rFALSE\r"
