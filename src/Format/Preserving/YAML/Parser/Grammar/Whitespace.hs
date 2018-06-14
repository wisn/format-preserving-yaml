{-# LANGUAGE FlexibleContexts #-}

module Format.Preserving.YAML.Parser.Grammar.Whitespace
  ( sWhitespace
  , whitespace
  )
where

import Control.Applicative ((<|>))
import qualified Text.Parsec as P

import Format.Preserving.YAML.Parser.Token (Token (..))

-- | A tokenizer that capture ' ' or '\t'.
whitespace :: P.Stream s m Char => P.ParsecT s u m Token
whitespace = space <|> tab

-- | A tokenizer that capture ' '.
space :: P.Stream s m Char => P.ParsecT s u m Token
space = Space <$ P.char ' '

-- | A tokenizer that capture '\t'.
tab :: P.Stream s m Char => P.ParsecT s u m Token
tab = Tab <$ P.char '\t'

-- | A grammar that identify ' ' or '\t'.
sWhitespace :: P.Stream s m Char => P.ParsecT s u m Char
sWhitespace = sSpace <|> sTab

-- | A grammar that identify ' '.
sSpace :: P.Stream s m Char => P.ParsecT s u m Char
sSpace = P.char ' '

-- | A grammar that identify '\t'.
sTab :: P.Stream s m Char => P.ParsecT s u m Char
sTab = P.char '\t'
