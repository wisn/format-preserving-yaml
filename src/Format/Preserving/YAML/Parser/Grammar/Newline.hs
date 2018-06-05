{-# LANGUAGE FlexibleContexts #-}

module Format.Preserving.YAML.Parser.Grammar.Newline (bBreak) where

import Control.Applicative ((<|>), (<$))
import qualified Text.Parsec as P

import Format.Preserving.YAML.Parser.Token (Token (..))

-- | A parent grammar that handle all newline characters.
bBreak :: P.Stream s m Char => P.ParsecT s u m Token
bBreak = bLineFeed <|> bCarriageReturn

-- | A newline character `\n` grammar that used by UNIX and MacOS X.
bLineFeed :: P.Stream s m Char => P.ParsecT s u m Token
bLineFeed = LineFeed <$ P.char '\n'

-- | A newline character `\r` grammar that used by MacOS upto 9.x.
bCarriageReturn :: P.Stream s m Char => P.ParsecT s u m Token
bCarriageReturn = CarriageReturn <$ P.char '\r'
