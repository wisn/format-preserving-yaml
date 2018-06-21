{-# LANGUAGE BangPatterns, FlexibleContexts #-}

module Format.Preserving.YAML.Parser.Grammar.Common
  ( bNotStr
  , cBSeparator
  , cEof
  , cExp
  , cIndicator
  , cPrintable
  , cSign
  , strSign
  )
where

import Control.Applicative ((<|>))
import Data.Char (ord)
import qualified Text.Parsec as P

import Format.Preserving.YAML.Parser.Grammar.Newline (bNewline)
import Format.Preserving.YAML.Parser.Grammar.Whitespace (sWhitespace)
import Format.Preserving.YAML.Parser.Token (Token (..))

-- | A grammar for the acceptable unicode into the Str and Comment token.
-- It is based on the YAML 1.2 Spec at
-- <http://yaml.org/spec/1.2/spec.html#id2770814> covering 8-bit, 16-bit, and
-- 32-bit Unicode characters.
cPrintable :: P.Stream s m Char => P.ParsecT s u m Char
cPrintable = c8Bit <|> c16Bit <|> c32Bit

-- | A grammar that transform EOF into LineFeed.
cEof :: P.Stream s m Char => P.ParsecT s u m Char
cEof = P.eof *> pure '\n'

-- | A grammar that check whether this is a Str or not. This is non-breaking
-- grammar version. The result is valid when it is not a Str.
bNotStr :: P.Stream s m Char => P.ParsecT s u m Char
bNotStr = whitespace <|> cBSeparator
  where
  whitespace = (sWhitespace *> whitespace) <|> cBSeparator

-- | A grammar that identify separator character such as Newline, Whitespace,
-- and ':'.
cBSeparator :: P.Stream s m Char => P.ParsecT s u m Char
cBSeparator = bNewline <|> cEof <|> mapping
  where
  mapping = (P.char ':' *> mapping) <|> (sWhitespace <|> bNewline <|> cEof)

-- | A grammar that identify integer sign such as '+' or '-'.
cSign :: P.Stream s m Char => P.ParsecT s u m Char
cSign = P.char '-' <|> P.char '+'

-- | A grammar that just like cSign but String.
strSign :: P.Stream s m Char => P.ParsecT s u m String
strSign = P.string "-" <|> P.string "+"

-- | A grammar that identify exponent character.
cExp :: P.Stream s m Char => P.ParsecT s u m Char
cExp = P.char 'e' <|> P.char 'E'

-- | A grammar for an indicator of content separator.
cIndicator :: P.Stream s m Char => P.ParsecT s u m Char
cIndicator =  P.char '-' <|> P.char '?' <|> P.char ':' <|> P.char ',' <|>
              P.char '[' <|> P.char ']' <|> P.char '{' <|> P.char '}' <|>
              P.char '#' <|> P.char '&' <|> P.char '*' <|> P.char '!' <|>
              P.char '|' <|> P.char '>' <|> P.char '%' <|> P.char '@' <|>
              P.char '`' <|> P.char '\'' <|> P.char '\"'

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
