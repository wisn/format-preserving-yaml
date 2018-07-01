{-# LANGUAGE OverloadedStrings #-}

module Spec12.Parser.ScalarSpec where

import Test.Hspec

import Format.Preserving.YAML (parseScalar)
import Format.Preserving.YAML.Spec12.Types (Scalar (..), (=!>), (-!>))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  context "with Whitespace" $ do
    it "returns single Scalar with Whitespace before" $ do
      parseScalar "\t \r\n \nnull"
      `shouldBe`
      (Right (Null $ ((=!>) "null") -!> "\t \r\n \n"))
  context "with Comment" $ do
    it "returns single Comment" $ do
      parseScalar "# YAML 1.2"
      `shouldBe`
      (Right (Comment $ (=!>) " YAML 1.2"))
  context "with Bool" $ do
    it "returns single True (true)" $ do
      parseScalar "true"
      `shouldBe`
      (Right (Bool True $ (=!>) "true"))
    it "returns single True (True)" $ do
      parseScalar "True"
      `shouldBe`
      (Right (Bool True $ (=!>) "True"))
    it "returns single True (TRUE)" $ do
      parseScalar "TRUE"
      `shouldBe`
      (Right (Bool True $ (=!>) "TRUE"))
    it "returns single False (false)" $ do
      parseScalar "false"
      `shouldBe`
      (Right (Bool False $ (=!>) "false"))
    it "returns single False (False)" $ do
      parseScalar "False"
      `shouldBe`
      (Right (Bool False $ (=!>) "False"))
    it "returns single False (FALSE)" $ do
      parseScalar "FALSE"
      `shouldBe`
      (Right (Bool False $ (=!>) "FALSE"))
  context "with Null" $ do
    it "returns single Null (null)" $ do
      parseScalar "null"
      `shouldBe`
      (Right (Null $ (=!>) "null"))
    it "returns single Null (Null)" $ do
      parseScalar "Null"
      `shouldBe`
      (Right (Null $ (=!>) "Null"))
    it "returns single Null (NULL)" $ do
      parseScalar "NULL"
      `shouldBe`
      (Right (Null $ (=!>) "NULL"))
  context "with Int" $ do
    it "returns single Int (+123)" $ do
      parseScalar "+123"
      `shouldBe`
      (Right (Int 123 $ (=!>) "+123"))
    it "returns single Int (-123)" $ do
      parseScalar "-123"
      `shouldBe`
      (Right (Int (-123) $ (=!>) "-123"))
  context "with Octal" $ do
    it "returns single Octal (0o123)" $ do
      parseScalar "0o123"
      `shouldBe`
      (Right (Octal "123" $ (=!>) "123"))
  context "with Hexadecimal" $ do
    it "returns single Hexadecimal (0x123aF)" $ do
      parseScalar "0x123aF"
      `shouldBe`
      (Right (Hexadecimal "123aF" $ (=!>) "123aF"))
  context "with Inf" $ do
    it "returns single Inf (.inf)" $ do
      parseScalar ".inf"
      `shouldBe`
      (Right (Inf $ (=!>) ".inf"))
    it "returns single Inf (+.Inf)" $ do
      parseScalar "+.Inf"
      `shouldBe`
      (Right (Inf $ (=!>) "+.Inf"))
    it "returns single Inf (-.INF)" $ do
      parseScalar "-.INF"
      `shouldBe`
      (Right (Inf $ (=!>) "-.INF"))
  context "with Nan" $ do
    it "returns single Nan (.nan)" $ do
      parseScalar ".nan"
      `shouldBe`
      (Right (NaN $ (=!>) ".nan"))
    it "returns single Nan (.NaN)" $ do
      parseScalar ".NaN"
      `shouldBe`
      (Right (NaN $ (=!>) ".NaN"))
    it "returns single Nan (.NAN)" $ do
      parseScalar ".NAN"
      `shouldBe`
      (Right (NaN $ (=!>) ".NAN"))
