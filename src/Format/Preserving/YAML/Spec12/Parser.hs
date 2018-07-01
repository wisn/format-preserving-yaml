{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}

module Format.Preserving.YAML.Spec12.Parser (parseScalar) where

import Data.Text (Text)
import Format.Preserving.YAML.Spec12.Grammar (scalar)
import Format.Preserving.YAML.Spec12.Types (Scalar)
import qualified Text.Parsec as P (ParseError, parse)

parseScalar :: Text -> Either P.ParseError Scalar
parseScalar = P.parse scalar []
