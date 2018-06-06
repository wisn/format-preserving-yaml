{-# LANGUAGE FlexibleContexts #-}

module Format.Preserving.YAML.Parser.Grammar.Boolean (nbBoolean) where

import Control.Applicative ((<|>), (<$), (*>))
import qualified Text.Parsec as P

import Format.Preserving.YAML.Parser.Grammar.Common (eof, newline)
import Format.Preserving.YAML.Parser.Token (Token (..))

-- | A grammar for capturing Bool content.
--
-- A boolean in YAML represent as true, false, True, False, TRUE, or FALSE.
-- Consecutive Bool will be considered as Str. Several accepted Bool are listed
-- below as a sequence of Bool.
--
-- [true, True, TRUE, false, False, FALSE, true: False]
--
-- A single Bool could be formed such as:
--
-- ```true```, ```false:```, ```false     :```, or ```  true    ```
nbBoolean :: P.Stream s m Char => P.ParsecT s u m Token
nbBoolean = true <|> false
  where
    true = Bool True <$ (nbTrue *> P.lookAhead nbNotStr)
    false = Bool False <$ (nbFalse *> P.lookAhead nbNotStr)

-- | A combinator for True content. Accepting all True form.
nbTrue :: P.Stream s m Char => P.ParsecT s u m String
nbTrue = P.string "true" <|> P.string "True" <|> P.string "TRUE"

-- | A combinator for False content. Accepting all False form.
nbFalse :: P.Stream s m Char => P.ParsecT s u m String
nbFalse = P.string "false" <|> P.string "False" <|> P.string "FALSE"

-- | A combinator for seeking the content separator.
nbNotStr :: P.Stream s m Char => P.ParsecT s u m Char
nbNotStr = separator <|> whitespace
  where
    separator = eof <|> newline <|> P.char ':'
    whitespace = P.lookAhead $ (P.char ' ' *> whitespace) <|> separator
