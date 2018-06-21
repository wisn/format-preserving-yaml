{-# LANGUAGE OverloadedStrings #-}

module Parser.FloatingSpec where

import Test.Hspec

import Format.Preserving.YAML (parse)
import Format.Preserving.YAML.Parser.Token (Token (..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  context "with Float token" $ do
    it "returns single Float (0.) token" $ do
      parse "0." `shouldBe` (Right [Float "0."])
    it "returns single Float (+0.e5) token" $ do
      parse "+0.e5" `shouldBe` (Right [Float "+0.e5"])
    it "returns single Float (.9) token" $ do
      parse ".9" `shouldBe` (Right [Float ".9"])
    it "returns single Float (-.9e10) token" $ do
      parse "-.9e10" `shouldBe` (Right [Float "-.9e10"])
    it "returns single Float (-12e03) token" $ do
      parse "-12e03" `shouldBe` (Right [Float "-12e03"])
    it "returns single Float (+12.1e10) token with Space" $ do
      parse "+12.1e10" `shouldBe` (Right [Float "+12.1e10", Space])
  context "with Inf token" $ do
    it "returns single Inf (inf) token" $ do
      parse ".inf" `shouldBe` (Right [Inf ".inf"])
    it "returns single Inf (-Inf) token" $ do
      parse "-.Inf" `shouldBe` (Right [Inf "-.Inf"])
    it "returns single Inf (INF) token" $ do
      parse ".INF" `shouldBe` (Right [Inf ".INF"])
    it "returns single Inf (+INF) token" $ do
      parse "+.INF" `shouldBe` (Right [Inf "+.INF"])
  context "with Nan token" $ do
    it "returns single Nan (nan) token" $ do
      parse ".nan" `shouldBe` (Right [Nan "nan"])
    it "returns single Nan (NaN) token" $ do
      parse ".NaN" `shouldBe` (Right [Nan "NaN"])
    it "returns single Nan (NAN) token" $ do
      parse ".NAN" `shouldBe` (Right [Nan "NAN"])
