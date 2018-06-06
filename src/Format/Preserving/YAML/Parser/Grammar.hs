{-# LANGUAGE FlexibleContexts #-}

module Format.Preserving.YAML.Parser.Grammar (yaml) where

import Control.Applicative ((<|>))
import qualified Text.Parsec as P

import Format.Preserving.YAML.Parser.Grammar.Boolean (nbBoolean)
import Format.Preserving.YAML.Parser.Grammar.Comment (cNbCommentText)
import Format.Preserving.YAML.Parser.Grammar.Newline (bBreak)
import Format.Preserving.YAML.Parser.Grammar.String (nbChar)
import Format.Preserving.YAML.Parser.Grammar.Whitespace (sWhite)
import Format.Preserving.YAML.Parser.Token (Tokens)

-- | A parent grammar that handle all possible YAML content.
yaml :: P.Stream s m Char => P.ParsecT s u m Tokens
yaml =  P.many $  bBreak
              <|> cNbCommentText
              <|> sWhite
              <|> P.try nbBoolean
              <|> nbChar
