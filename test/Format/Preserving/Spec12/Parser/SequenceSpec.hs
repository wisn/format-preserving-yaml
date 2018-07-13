{-# LANGUAGE OverloadedStrings #-}

module Spec12.Parser.SequenceSpec where

import Test.Hspec

import Format.Preserving.YAML (parseSequence)
import Format.Preserving.YAML.Spec12.Types
  ( Node (..)
  , Scalar (..)
  , Sequence
  , (=!>)
  )

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  context "with inline Sequence" $ do
    it "returns empty Sequence" $ do
      parseSequence "[]"
      `shouldBe`
      (Right (Sequence []))
    it "returns single Scalar element Sequence" $ do
      parseSequence "[true]"
      `shouldBe`
      (Right (Sequence [Scalar $ Bool True $ (=!>) "true"]))
    it "returns two Scalar elements Sequence" $ do
      parseSequence "[.inf, 123]"
      `shouldBe`
      (Right (Sequence [
        Scalar (Inf $ (=!>) ".inf"), Scalar (Int 123 $ (=!>) "123")
      ]))
    it "returns nested Sequence" $ do
      parseSequence "[990, ['str']]"
      `shouldBe`
      (Right (Sequence [
        Scalar (Int 990 $ (=!>) "990"), Sequence [
          Scalar (SingleQuoted "str" $ (=!>) "str")
        ]
      ]))
