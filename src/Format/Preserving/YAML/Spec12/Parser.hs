{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}

module Format.Preserving.YAML.Spec12.Parser
  ( parseDirective
  , parseScalar
  , parseSequence
  )
where

import Prelude hiding (sequence)

import Data.Text (Text)
import Format.Preserving.YAML.Spec12.Grammar (directive, scalar, sequence)
import Format.Preserving.YAML.Spec12.Types (Directive, Node)
import qualified Text.Parsec as P (ParseError, parse)

parseDirective :: Text -> Either P.ParseError Directive
parseDirective = P.parse directive []

parseScalar :: Text -> Either P.ParseError Node
parseScalar = P.parse scalar []

parseSequence :: Text -> Either P.ParseError Node
parseSequence = P.parse sequence []
