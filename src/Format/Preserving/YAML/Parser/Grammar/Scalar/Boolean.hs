{-# LANGUAGE FlexibleContexts #-}

module Format.Preserving.YAML.Parser.Grammar.Scalar.Boolean
  ( bool
  , nbBoolean
  )
where

import Control.Applicative ((<|>))
import qualified Data.Text as T (pack)
import qualified Text.Parsec as P

import Format.Preserving.YAML.Parser.Grammar.Common (bNotStr, cEof)
import Format.Preserving.YAML.Parser.Grammar.Newline (bNewline)
import Format.Preserving.YAML.Parser.Grammar.Whitespace (sWhitespace)
import Format.Preserving.YAML.Parser.Token (Token (..))

-- | A tokenizer that capture Bool content.
--
-- A boolean in YAML represent as true, false, True, False, TRUE, or FALSE.
-- Consecutive Bool will be considered as Str. Several accepted Bool are listed
-- below as a sequence of Bool.
--
-- [true, True, false, FALSE, TRUE: False]
bool :: P.Stream s m Char => P.ParsecT s u m Token
bool = true <|> false

-- | A tokenizer that capture True content.
true :: P.Stream s m Char => P.ParsecT s u m Token
true = Bool True. T.pack <$> nbTrue

-- | A tokenizer that capture False content.
false :: P.Stream s m Char => P.ParsecT s u m Token
false = Bool False. T.pack <$> nbFalse

-- | A grammar that identify Bool content such as True and False.
nbBoolean :: P.Stream s m Char => P.ParsecT s u m String
nbBoolean = nbTrue <|> nbFalse

-- | A grammar that identify all True content form.
nbTrue :: P.Stream s m Char => P.ParsecT s u m String
nbTrue = P.lookAhead true *> P.manyTill P.anyChar (P.lookAhead bNotStr)
  where
  true = P.string "true" <|> P.try (P.string "True") <|> P.string "TRUE"

-- | A grammar that identify all False content form.
nbFalse :: P.Stream s m Char => P.ParsecT s u m String
nbFalse = P.lookAhead false *> P.manyTill P.anyChar (P.lookAhead bNotStr)
  where
  false = P.string "false" <|> P.try (P.string "False") <|> P.string "FALSE"
