{-# LANGUAGE FlexibleContexts #-}

module Format.Preserving.YAML.Parser.Grammar.Scalar.String (nbChar) where

import Control.Applicative ((<|>))
import qualified Data.Text as T (pack)
import qualified Text.Parsec as P

import Format.Preserving.YAML.Parser.Grammar.Common (cEof, cPrintable)
import Format.Preserving.YAML.Parser.Grammar.Newline (bNewline)
import Format.Preserving.YAML.Parser.Token

-- | Undocumented grammar.
nbChar :: P.Stream s m Char => P.ParsecT s u m Token
nbChar =
  let collect = Str. T.pack <$> P.manyTill cPrintable end
      end     = P.lookAhead $ bNewline <|> cEof
  in P.lookAhead cPrintable *> collect
