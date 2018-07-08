{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}

module Format.Preserving.YAML.Spec12.Parser
  ( parseDirective
  , parseScalar
  )
where

import Data.Text (Text)
import Format.Preserving.YAML.Spec12.Grammar (directive, scalar)
import Format.Preserving.YAML.Spec12.Types (Directive, Scalar)
import qualified Text.Parsec as P (ParseError, parse)

parseDirective :: Text -> Either P.ParseError Directive
parseDirective = P.parse directive []

parseScalar :: Text -> Either P.ParseError Scalar
parseScalar = P.parse scalar []
