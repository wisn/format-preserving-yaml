{-# LANGUAGE FlexibleContexts #-}

module Format.Preserving.YAML.Parser.Grammar.Comment where

import Control.Applicative ((<|>))
import qualified Data.Text as T (pack)
import qualified Text.Parsec as P

import Format.Preserving.YAML.Parser.Grammar.Common (eof, newline)
import Format.Preserving.YAML.Parser.Token (Token (..))

-- | A grammar for collecting YAML comment that starting and ending with a
-- non-break character.
cNbCommentText :: P.Stream s m Char => P.ParsecT s u m Token
cNbCommentText =
    let collect = Comment. T.pack <$> P.manyTill P.anyChar endline
        endline = P.lookAhead (newline <|> eof)
    in P.char '#' *> collect
