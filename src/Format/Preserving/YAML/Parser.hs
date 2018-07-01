{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Format.Preserving.YAML.Parser where

import Data.Text (Text)
import qualified Text.Parsec as P (ParseError, parse)

-- import Format.Preserving.YAML.Parser.Grammar (yaml)
-- import Format.Preserving.YAML.Parser.Token (Tokens)
--
-- -- | The main parser to parsing YAML.
-- parse :: Text -> Either P.ParseError Tokens
-- parse = P.parse yaml []
