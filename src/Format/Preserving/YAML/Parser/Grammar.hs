{-# LANGUAGE FlexibleContexts #-}

module Format.Preserving.YAML.Parser.Grammar (yaml) where

import Control.Applicative ((<|>))
import qualified Text.Parsec as P

import Format.Preserving.YAML.Parser.Grammar.Comment (comment)
import Format.Preserving.YAML.Parser.Grammar.Newline (newline)
import Format.Preserving.YAML.Parser.Grammar.Scalar (scalar)
import Format.Preserving.YAML.Parser.Grammar.Whitespace (whitespace)
import Format.Preserving.YAML.Parser.Token (Tokens)

-- | A YAML tokenizer.
yaml :: P.Stream s m Char => P.ParsecT s u m Tokens
yaml =  content

-- | A YAML content tokenizer that capture all possible YAML content.
content :: P.Stream s m Char => P.ParsecT s u m Tokens
content = P.many $ whitespace <|> newline <|> comment <|> scalar
