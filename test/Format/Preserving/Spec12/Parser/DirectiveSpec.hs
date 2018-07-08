{-# LANGUAGE OverloadedStrings #-}

module Spec12.Parser.DirectiveSpec where

import Test.Hspec

import Format.Preserving.YAML (parseDirective)
import Format.Preserving.YAML.Spec12.Types
  ( Directive (..)
  , Handle (..)
  , Version (..)
  , (=!>)
  , (~|||>)
  )

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  context "with Version" $ do
    it "returns single YAML Default Version" $ do
      parseDirective "%YAML 1.2"
      `shouldBe`
      (Right $ (Version Default $ (=!>) "%YAML 1.2") ~|||> [" "])
    it "returns single YAML UserDefined Version" $ do
      parseDirective "%YAML  1.1"
      `shouldBe`
      (Right $ (Version (UserDefined "1.1") $ (=!>) "%YAML  1.1") ~|||> ["  "])
  context "with Tag" $ do
    it "returns single YAML Primary Tag" $ do
      parseDirective "%TAG ! primary-tag:"
      `shouldBe`
      (Right $
        (Tag (PrimaryTag, "primary-tag:") $ (=!>) "%TAG ! primary-tag:")
        ~|||> [" ", " "]
      )
    it "returns single YAML Secondary Tag" $ do
      parseDirective "%TAG !!  secondary-tag:"
      `shouldBe`
      (Right $
        (Tag (SecondaryTag, "secondary-tag:") $
          (=!>) "%TAG !!  secondary-tag:"
        )
        ~|||> [" ", "  "]
      )
    it "returns single YAML Named Tag" $ do
      parseDirective "%TAG  !e! named-tag:"
      `shouldBe`
      (Right $
        (Tag (NamedTag "e", "named-tag:") $
          (=!>) "%TAG  !e! named-tag:"
        )
        ~|||> ["  ", " "]
      )
  context "with Ignored" $ do
    it "returns single YAML none-reserved Directive" $ do
      parseDirective "%FOO bar baz"
      `shouldBe`
      (Right (Ignored "FOO bar baz" $ (=!>) "%FOO bar baz"))
