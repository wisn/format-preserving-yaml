{-# LANGUAGE FlexibleContexts #-}

module Format.Preserving.YAML.Parser.Grammar.Scalar.Null where

import Control.Applicative ((<|>))
import qualified Data.Text as T (pack)
import qualified Text.Parsec as P

import Format.Preserving.YAML.Parser.Grammar.Common (bNotStr, cEof)
import Format.Preserving.YAML.Parser.Grammar.Newline (bNewline)
import Format.Preserving.YAML.Parser.Grammar.Whitespace (sWhitespace)
import Format.Preserving.YAML.Parser.Token (Token (..))

-- | A tokenizer that capture Null content.
--
-- A boolean in YAML represent as null, Null, NULL, '~', or ''.
-- Consecutive Null will be considered as Str. Several accepted Null are listed
-- below as a sequence of Null.
--
-- [null, Null: NULL, ~]
yNull :: P.Stream s m Char => P.ParsecT s u m Token
yNull = P.lookAhead nbNull *> collect
  where
  collect = Null. T.pack <$> P.manyTill P.anyChar (P.lookAhead bNotStr)

-- | A grammar that identify Null content.
nbNull :: P.Stream s m Char => P.ParsecT s u m String
nbNull = P.string "null" <|> P.string "Null" <|> P.string "NULL" <|>
         P.string "~"
