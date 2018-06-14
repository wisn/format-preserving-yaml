{-# LANGUAGE FlexibleContexts #-}

module Format.Preserving.YAML.Parser.Grammar.Scalar.Null
  ( yNull
  , nbNull
  )
where

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
yNull =
  let collect = Null. T.pack <$> P.manyTill P.anyChar endline
      endline = P.lookAhead (sWhitespace <|> bNewline <|> cEof)
  in nbNull *> collect

-- | A grammar that identify Null content.
nbNull :: P.Stream s m Char => P.ParsecT s u m Char
nbNull = P.lookAhead (strNull <|> chrNull)

-- | A grammar that identify null, Null, NULL content form.
strNull :: P.Stream s m Char => P.ParsecT s u m Char
strNull = (P.string "null" <|> P.string "Null" <|> P.string "NULL") *> bNotStr

-- | A grammar that identify '~' Null form.
chrNull :: P.Stream s m Char => P.ParsecT s u m Char
chrNull = P.char '~' *> bNotStr
