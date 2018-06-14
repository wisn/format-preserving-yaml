{-# LANGUAGE OverloadedStrings #-}

module Parser.BooleanSpec where

import Test.Hspec

import Format.Preserving.YAML (parse)
import Format.Preserving.YAML.Parser.Token (Token (..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  context "with True token" $ do
    it "returns single Bool (true) token" $ do
      parse "true" `shouldBe` (Right [Bool True "true"])
    it "returns Bool (True) token between Space" $ do
      parse " True " `shouldBe` (Right [Space, Bool True "True", Space])
    it "returns Bool (TRUE) token between Newline" $ do
      parse "\n\tTRUE\n"
      `shouldBe`
      (Right [LineFeed, Tab, Bool True "TRUE", LineFeed])
    it "returns Str token instead of Bool" $ do
      parse "true  :s" `shouldBe` (Right [Str "true  :s"])
  context "with False token" $ do
    it "returns single Bool (false) token" $ do
      parse "false" `shouldBe` (Right [Bool False "false"])
    it "returns Bool (False) token between Space" $ do
      parse " False " `shouldBe` (Right [Space, Bool False "False", Space])
    it "returns Bool (FALSE) token between Newline" $ do
      parse "\rFALSE\r"
      `shouldBe`
      (Right [CarriageReturn, Bool False "FALSE", CarriageReturn])
    it "returns Str token instead of Bool" $ do
      parse "false\ntrue" `shouldBe` (Right [Str "false\ntrue"])
