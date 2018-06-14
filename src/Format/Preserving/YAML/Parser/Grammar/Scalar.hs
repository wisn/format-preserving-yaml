{-# LANGUAGE FlexibleContexts #-}

module Format.Preserving.YAML.Parser.Grammar.Scalar where

import Control.Applicative ((<|>))
import qualified Text.Parsec as P

import Format.Preserving.YAML.Parser.Grammar.Scalar.Boolean (bool)
import Format.Preserving.YAML.Parser.Grammar.Scalar.Floating (inf, nan)
import Format.Preserving.YAML.Parser.Grammar.Scalar.Integer
  ( int
  , hexadecimal
  , octal
  )
import Format.Preserving.YAML.Parser.Grammar.Scalar.Null (yNull)
import Format.Preserving.YAML.Parser.Grammar.Scalar.String (nbChar)
import Format.Preserving.YAML.Parser.Token (Token (..))

-- | A tokenizer that capture YAML Scalar such as Bool, Int, Str, and so on.
scalar :: P.Stream s m Char => P.ParsecT s u m Token
scalar =  P.try yNull
      <|> P.try bool
      <|> P.try int <|> P.try octal <|> P.try hexadecimal
      <|> P.try inf <|> P.try nan
      <|> nbChar
