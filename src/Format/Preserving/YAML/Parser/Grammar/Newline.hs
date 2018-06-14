{-# LANGUAGE FlexibleContexts #-}

module Format.Preserving.YAML.Parser.Grammar.Newline
  ( newline
  , bNewline
  )
where

import Control.Applicative ((<|>))
import qualified Text.Parsec as P

import Format.Preserving.YAML.Parser.Token (Token (..))

-- | A tokenizer that capture '\n' or '\r'. DOS and Windows `\r\n` will be
-- captured as `[CarriageReturn, LineFeed]` tokens.
newline :: P.Stream s m Char => P.ParsecT s u m Token
newline = lineFeed <|> carriageReturn

-- | A tokenizer that capture `\n` which is used by UNIX and MacOS X.
lineFeed :: P.Stream s m Char => P.ParsecT s u m Token
lineFeed = LineFeed <$ P.char '\n'

-- | A tokenizer that capture `\r` which is used by MacOS upto 9.x.
carriageReturn :: P.Stream s m Char => P.ParsecT s u m Token
carriageReturn = CarriageReturn <$ P.char '\r'

-- | A grammar that identify '\n' or '\r'. DOS and Windows `\r\n` will be
-- identified by bCarriageReturn grammar.
bNewline :: P.Stream s m Char => P.ParsecT s u m Char
bNewline = bLineFeed <|> bCarriageReturn

-- | A grammar that identify '\n' which is used by UNIX and MacOS X.
bLineFeed :: P.Stream s m Char => P.ParsecT s u m Char
bLineFeed = P.char '\n'

-- | A grammar that identify '\r' which is used by MacOS upto 9.x.
bCarriageReturn :: P.Stream s m Char => P.ParsecT s u m Char
bCarriageReturn = P.char '\r'
