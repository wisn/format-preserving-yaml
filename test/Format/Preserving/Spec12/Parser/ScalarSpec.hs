{-# LANGUAGE OverloadedStrings #-}

module Spec12.Parser.ScalarSpec where

import Test.Hspec

import Data.Scientific (Scientific)
import Format.Preserving.YAML (parseScalar)
import Format.Preserving.YAML.Spec12.Types
  ( Node (..)
  , Scalar (..)
  , (=!>)
  , (-!>)
  )

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  context "with Whitespace" $ do
    it "returns single Scalar with Whitespace before" $ do
      parseScalar "\t \r\n \nnull"
      `shouldBe`
      (Right (Scalar $ Null $ ((=!>) "null") -!> "\t \r\n \n"))
  context "with Comment" $ do
    it "returns single Comment" $ do
      parseScalar "# YAML 1.2"
      `shouldBe`
      (Right (Scalar $ Comment $ (=!>) " YAML 1.2"))
  context "with Bool" $ do
    it "returns single True (true)" $ do
      parseScalar "true"
      `shouldBe`
      (Right (Scalar $ Bool True $ (=!>) "true"))
    it "returns single True (True)" $ do
      parseScalar "True"
      `shouldBe`
      (Right (Scalar $ Bool True $ (=!>) "True"))
    it "returns single True (TRUE)" $ do
      parseScalar "TRUE"
      `shouldBe`
      (Right (Scalar $ Bool True $ (=!>) "TRUE"))
    it "returns single False (false)" $ do
      parseScalar "false"
      `shouldBe`
      (Right (Scalar $ Bool False $ (=!>) "false"))
    it "returns single False (False)" $ do
      parseScalar "False"
      `shouldBe`
      (Right (Scalar $ Bool False $ (=!>) "False"))
    it "returns single False (FALSE)" $ do
      parseScalar "FALSE"
      `shouldBe`
      (Right (Scalar $ Bool False $ (=!>) "FALSE"))
  context "with Null" $ do
    it "returns single Null (null)" $ do
      parseScalar "null"
      `shouldBe`
      (Right (Scalar $ Null $ (=!>) "null"))
    it "returns single Null (Null)" $ do
      parseScalar "Null"
      `shouldBe`
      (Right (Scalar $ Null $ (=!>) "Null"))
    it "returns single Null (NULL)" $ do
      parseScalar "NULL"
      `shouldBe`
      (Right (Scalar $ Null $ (=!>) "NULL"))
  context "with Int" $ do
    it "returns single Int (123)" $ do
      parseScalar "123"
      `shouldBe`
      (Right (Scalar $ Int 123 $ (=!>) "123"))
    it "returns single Int (+123)" $ do
      parseScalar "+123"
      `shouldBe`
      (Right (Scalar $ Int 123 $ (=!>) "+123"))
    it "returns single Int (-123)" $ do
      parseScalar "-123"
      `shouldBe`
      (Right (Scalar $ Int (-123) $ (=!>) "-123"))
  context "with Octal" $ do
    it "returns single Octal (0o123)" $ do
      parseScalar "0o123"
      `shouldBe`
      (Right (Scalar $ Octal "123" $ (=!>) "123"))
  context "with Hexadecimal" $ do
    it "returns single Hexadecimal (0x123aF)" $ do
      parseScalar "0x123aF"
      `shouldBe`
      (Right (Scalar $ Hexadecimal "123aF" $ (=!>) "123aF"))
  context "with Float" $ do
    it "returns single Float (0.)" $ do
      parseScalar "0."
      `shouldBe`
      (Right (Scalar $ Float (0.0 :: Scientific) $ (=!>) "0."))
    it "returns single Float (-0.0)" $ do
      parseScalar "-0.0"
      `shouldBe`
      (Right (Scalar $ Float ((-0.0) :: Scientific) $ (=!>) "-0.0"))
    it "returns single Float (.5)" $ do
      parseScalar ".5"
      `shouldBe`
      (Right (Scalar $ Float (0.5 :: Scientific) $ (=!>) ".5"))
    it "returns single Float (+12e03)" $ do
      parseScalar "+12e03"
      `shouldBe`
      (Right (Scalar $ Float (12e03 :: Scientific) $ (=!>) "+12e03"))
    it "returns single Float (-2E+05)" $ do
      parseScalar "-2E+05"
      `shouldBe`
      (Right (Scalar $ Float ((-2e+05) :: Scientific) $ (=!>) "-2E+05"))
  context "with Inf" $ do
    it "returns single Inf (.inf)" $ do
      parseScalar ".inf"
      `shouldBe`
      (Right (Scalar $ Inf $ (=!>) ".inf"))
    it "returns single Inf (+.Inf)" $ do
      parseScalar "+.Inf"
      `shouldBe`
      (Right (Scalar $ Inf $ (=!>) "+.Inf"))
    it "returns single Inf (-.INF)" $ do
      parseScalar "-.INF"
      `shouldBe`
      (Right (Scalar $ Inf $ (=!>) "-.INF"))
  context "with Nan" $ do
    it "returns single Nan (.nan)" $ do
      parseScalar ".nan"
      `shouldBe`
      (Right (Scalar $ NaN $ (=!>) ".nan"))
    it "returns single Nan (.NaN)" $ do
      parseScalar ".NaN"
      `shouldBe`
      (Right (Scalar $ NaN $ (=!>) ".NaN"))
    it "returns single Nan (.NAN)" $ do
      parseScalar ".NAN"
      `shouldBe`
      (Right (Scalar $ NaN $ (=!>) ".NAN"))
  context "with SingleQuoted" $ do
    it "returns single SingleQuoted ('s\\n\\tt  \\'\"\"r')" $ do
      parseScalar "'s\n\tt  \'\"\"r'"
      `shouldBe`
      (Right (Scalar $
        SingleQuoted "s\n\tt  \'\"\"r" $ (=!>) "s\n\tt  \'\"\"r"
      ))
  context "with DoubleQuoted" $ do
    it "returns single DoubleQuoted (\"s\\n\\tt  ''\\\"r\")" $ do
      parseScalar "\"s\n\tt  ''\\\"r\""
      `shouldBe`
      (Right (Scalar $
        DoubleQuoted "s\n\tt  ''\\\"r" $ (=!>) "s\n\tt  ''\\\"r"
      ))
  context "with Alias" $ do
    it "returns single Alias" $ do
      parseScalar "*aL1as_"
      `shouldBe`
      (Right (Scalar $ Alias "aL1as_" $ (=!>) "aL1as_"))
