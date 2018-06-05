{-# LANGUAGE FlexibleContexts #-}

module Format.Preserving.YAML.Parser.Grammar.Whitespace (sWhite) where

import Control.Applicative ((<|>))
import qualified Text.Parsec as P

import Format.Preserving.YAML.Parser.Token (Token (..))

-- | A parent grammar that handle both whitespace and tab character.
sWhite :: P.Stream s m Char => P.ParsecT s u m Token
sWhite = sSpace <|> sTab

-- | A grammar for capturing whitespace character.
sSpace :: P.Stream s m Char => P.ParsecT s u m Token
sSpace = Space <$ P.char ' '

-- | A grammar for capturing tab character.
sTab :: P.Stream s m Char => P.ParsecT s u m Token
sTab = Tab <$ P.char '\t'
