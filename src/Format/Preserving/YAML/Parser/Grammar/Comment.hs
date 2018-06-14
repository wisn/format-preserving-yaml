{-# LANGUAGE FlexibleContexts #-}

module Format.Preserving.YAML.Parser.Grammar.Comment where

import Control.Applicative ((<|>))
import qualified Data.Text as T (pack)
import qualified Text.Parsec as P

import Format.Preserving.YAML.Parser.Grammar.Common (cEof)
import Format.Preserving.YAML.Parser.Grammar.Newline (bNewline)
import Format.Preserving.YAML.Parser.Token (Token (..))

-- | A tokenizer that capture Comment content.
comment :: P.Stream s m Char => P.ParsecT s u m Token
comment =
  let collect = Comment. T.pack <$> P.manyTill P.anyChar endline
      endline = P.lookAhead (bNewline <|> cEof)
  in nbComment *> collect

-- | A grammar that identify Comment content.
nbComment :: P.Stream s m Char => P.ParsecT s u m Char
nbComment = P.char '#'
