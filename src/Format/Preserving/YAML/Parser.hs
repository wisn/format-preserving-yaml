{-# LANGUAGE FlexibleInstances #-}

module Format.Preserving.YAML.Parser (parse) where

import qualified Data.Text as T (Text, pack)
import qualified Text.Parsec as P (ParseError, parse)

import Format.Preserving.YAML.Parser.Grammar (yaml)
import Format.Preserving.YAML.Parser.Token

class Parser a where
    parse :: a -> Either P.ParseError Token

-- | An instance that packing String to Text then parse the YAML content.
instance Parser String where
    parse = parseYAML. T.pack

-- | The main parser to parsing YAML.
parseYAML :: T.Text -> Either P.ParseError Token
parseYAML = P.parse yaml []
