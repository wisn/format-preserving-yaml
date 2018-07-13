{-# LANGUAGE FlexibleContexts #-}

module Format.Preserving.YAML.Spec12.Grammar.Sequence where

import Prelude hiding (sequence)

import Control.Applicative ((<|>))
import Data.Text (Text)
import Format.Preserving.YAML.Spec12.Grammar.Scalar (scalar)
import Format.Preserving.YAML.Spec12.Types (Node (..), Scalar (..), Sequence)
import qualified Text.Parsec as P

-- | A grammar for parsing a YAML Sequence. Unfortunately, nested sequence
-- still not supported.
sequence :: P.Parsec Text () Node
sequence = do
  P.char '['
  content <- P.sepBy (P.try sequence <|> scalar) (P.char ',')
  P.char ']'

  return $ Sequence content
