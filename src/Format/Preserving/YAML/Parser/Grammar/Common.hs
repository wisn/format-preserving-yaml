{-# LANGUAGE BangPatterns, FlexibleContexts #-}

module Format.Preserving.YAML.Parser.Grammar.Common
  ( cIndicator
  , cPrintable
  , eof
  , newline
  )
where

import Control.Applicative ((<|>))
import Data.Char (ord)
import qualified Text.Parsec as P

import Format.Preserving.YAML.Parser.Token (Token (..))

-- | A grammar for the acceptable unicode into the Str and Comment token.
-- It is based on the YAML 1.2 Spec at
-- <http://yaml.org/spec/1.2/spec.html#id2770814> covering 8-bit, 16-bit, and
-- 32-bit Unicode characters.
cPrintable :: P.Stream s m Char => P.ParsecT s u m Char
cPrintable = c8Bit <|> c16Bit <|> c32Bit

-- | A grammar for an indicator of content separator.
cIndicator :: P.Stream s m Char => P.ParsecT s u m Char
cIndicator =  P.char '-' <|> P.char '?' <|> P.char ':' <|> P.char ',' <|>
              P.char '[' <|> P.char ']' <|> P.char '{' <|> P.char '}' <|>
              P.char '#' <|> P.char '&' <|> P.char '*' <|> P.char '!' <|>
              P.char '|' <|> P.char '>' <|> P.char '%' <|> P.char '@' <|>
              P.char '`' <|> P.char '\'' <|> P.char '\"'

-- | A grammar that transform EOF into LineFeed.
eof :: P.Stream s m Char => P.ParsecT s u m Char
eof = P.eof *> pure '\n'

-- | A combinator grammar for newline characters.
newline :: P.Stream s m Char => P.ParsecT s u m Char
newline = P.char '\n' <|> P.char '\r'

-- | A grammar for 8-bit Unicode character. The accepted character are '\t',
-- '\n', '\r', or ' ' until '~'.
c8Bit :: P.Stream s m Char => P.ParsecT s u m Char
c8Bit = P.satisfy $ \c ->
    let !x = ord c
    in x == 0x9 || x == 0xA || x == 0xD || (0x20 <= x && x <= 0x7E)

-- | A grammar for 16-bit Unicode character. The accepted character are '\133',
-- '\160' until '\55295', or '\57344' until '\65533'.
c16Bit :: P.Stream s m Char => P.ParsecT s u m Char
c16Bit = P.satisfy $ \c ->
    let !x = ord c
    in x == 0x85 || (0xA0 <= x && x <= 0xD7FF) || (0xE000 <= x && x <= 0xFFFD)

-- | A grammar for 32-bit Unicode character. The accepted character are
-- '\65536' until '\69631'.
c32Bit :: P.Stream s m Char => P.ParsecT s u m Char
c32Bit = P.satisfy $ \c ->
    let !x = ord c
    in 0x10000 <= x && x <= 0x10FFFF
