{-# LANGUAGE FlexibleContexts #-}

module Format.Preserving.YAML.Parser.Grammar (yaml) where

import Control.Applicative ((<$), (<$>), (<|>), (<*>))
import qualified Data.Text as T
import qualified Text.Parsec as P

import Format.Preserving.YAML.Parser.Token

-- | A parent grammar that handle all possible YAML content.
yaml :: P.Stream s m Char => P.ParsecT s u m Token
yaml =  bBreak
    <|> cComment
    <|> sWhite
    <|> (EOF <$ P.eof)

-- | A newline character `\r` that used by MacOS upto 9.x. Capture and count
-- its consecutive character then back to `yaml` grammar.
bCarriageReturn :: P.Stream s m Char => P.ParsecT s u m Token
bCarriageReturn =
    let count = EOL. length <$> P.many1 (P.char '\r')
    in count <*> yaml

-- | A newline character `\n` that used by UNIX and MacOS X. Capture and count
-- its consecutive character then back to `yaml` grammar.
bLineFeed :: P.Stream s m Char => P.ParsecT s u m Token
bLineFeed =
    let count = EOL. length <$> P.many1 (P.char '\n')
    in count <*> yaml

-- TODO: Add DOS style newline `\r\n`.
-- | A parent grammar that handle all newline characters.
bBreak :: P.Stream s m Char => P.ParsecT s u m Token
bBreak =  bCarriageReturn
      <|> bLineFeed

cComment :: P.Stream s m Char => P.ParsecT s u m Token
cComment =
    let collect = Comment. T.pack <$> P.manyTill P.anyChar endline
        endline = P.lookAhead (P.char '\n')
    in P.char '#' *> collect <*> yaml

-- | Capture and count consecutive whitespace then back to `yaml` grammar.
sSpace :: P.Stream s m Char => P.ParsecT s u m Token
sSpace =
    let count = Spaces. length <$> P.many1 (P.char ' ')
    in count <*> yaml

-- | Capture and count consecutive tab then back to `yaml` grammar.
sTab :: P.Stream s m Char => P.ParsecT s u m Token
sTab =
    let count = Tabs. length <$> P.many1 (P.char '\t')
    in count <*> yaml

-- | A parent grammar that handle both whitespace and tab.
sWhite :: P.Stream s m Char => P.ParsecT s u m Token
sWhite = sSpace <|> sTab
