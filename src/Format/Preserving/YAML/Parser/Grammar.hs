{-# LANGUAGE FlexibleContexts #-}

module Format.Preserving.YAML.Parser.Grammar (yaml) where

import Control.Applicative ((<$), (<$>), (<|>), (*>))
import qualified Data.Text as T (pack)
import qualified Text.Parsec as P

import Format.Preserving.YAML.Parser.Token

-- | A parent grammar that handle all possible YAML content.
yaml :: P.Stream s m Char => P.ParsecT s u m Tokens
yaml =  P.many $  bBreak
              <|> cComment
              <|> sWhite

-- | A grammar transformer for EOF to LineFeed.
eof :: P.Stream s m Char => P.ParsecT s u m Char
eof = P.eof *> pure '\n'

-- | A combinator grammar for newline.
newline :: P.Stream s m Char => P.ParsecT s u m Char
newline = P.char '\n' <|> P.char '\r'

-- | A newline character `\r` grammar that used by MacOS upto 9.x
bCarriageReturn :: P.Stream s m Char => P.ParsecT s u m Token
bCarriageReturn = CarriageReturn <$ P.char '\r'

-- | A newline character `\n` grammar that used by UNIX and MacOS X.
bLineFeed :: P.Stream s m Char => P.ParsecT s u m Token
bLineFeed = LineFeed <$ P.char '\n'

-- | A parent grammar that handle all newline characters.
bBreak :: P.Stream s m Char => P.ParsecT s u m Token
bBreak = bCarriageReturn <|> bLineFeed

-- | A grammar for collecting single line comment.
cComment :: P.Stream s m Char => P.ParsecT s u m Token
cComment =
    let collect = Comment. T.pack <$> P.manyTill P.anyChar endline
        endline = P.lookAhead (newline <|> eof)
    in P.char '#' *> collect

-- | A grammar for capturing whitespace.
sSpace :: P.Stream s m Char => P.ParsecT s u m Token
sSpace = Space <$ P.char ' '

-- | A grammar for capturing tab.
sTab :: P.Stream s m Char => P.ParsecT s u m Token
sTab = Tab <$ P.char '\t'

-- | A parent grammar that handle both whitespace and tab.
sWhite :: P.Stream s m Char => P.ParsecT s u m Token
sWhite = sSpace <|> sTab
