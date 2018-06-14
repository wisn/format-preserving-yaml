{-# LANGUAGE FlexibleContexts #-}

module Format.Preserving.YAML.Parser.Grammar.Scalar.Floating where

import Control.Applicative ((<|>))
import qualified Data.Text as T (pack)
import qualified Text.Parsec as P

import Format.Preserving.YAML.Parser.Grammar.Common (bNotStr, cEof, cSign)
import Format.Preserving.YAML.Parser.Grammar.Newline (bNewline)
import Format.Preserving.YAML.Parser.Grammar.Whitespace (sWhitespace)
import Format.Preserving.YAML.Parser.Token (Token (..))

-- | A tokenizer that capture Inf content.
--
-- An Inf in YAML represent as `[-+]?\.(inf|Inf|INF)`. Inf is a part of Float
-- in YAML content.
inf :: P.Stream s m Char => P.ParsecT s u m Token
inf = Inf. T.pack <$> nbInf

-- | A grammar that identify Inf content.
nbInf :: P.Stream s m Char => P.ParsecT s u m String
nbInf = P.lookAhead (inf <|> (cSign *> inf)) *> P.manyTill P.anyChar endline
  where
  endline = P.lookAhead (sWhitespace <|> bNewline <|> cEof)
  inf     = P.char '.' *> strInf *> bNotStr
  strInf  = P.string "inf" <|> P.string "Inf" <|> P.string "INF"

-- | A tokenizer that capture Nan content.
--
-- A Nan in YAML represent as `\.(nan|NaN|NAN)`. Nan is a part of Float in YAML
-- content.
nan :: P.Stream s m Char => P.ParsecT s u m Token
nan = Nan. T.pack <$> nbNan

-- | A grammar that identify Nan content.
nbNan :: P.Stream s m Char => P.ParsecT s u m String
nbNan = P.char '.' *> P.lookAhead nan *> P.manyTill P.anyChar endline
  where
  endline = P.lookAhead (sWhitespace <|> bNewline <|> cEof)
  nan     = (P.string "nan" <|> P.string "NaN" <|> P.string "NAN") *> bNotStr
