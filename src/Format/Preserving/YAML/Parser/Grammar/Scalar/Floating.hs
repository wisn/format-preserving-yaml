{-# LANGUAGE FlexibleContexts #-}

module Format.Preserving.YAML.Parser.Grammar.Scalar.Floating where

import Control.Applicative ((<|>))
import Data.Monoid ((<>))
import qualified Data.Text as T (pack)
import qualified Text.Parsec as P

import Format.Preserving.YAML.Parser.Grammar.Common
  ( bNotStr
  , cEof
  , cExp
  , strSign
  )
import Format.Preserving.YAML.Parser.Grammar.Newline (bNewline)
import Format.Preserving.YAML.Parser.Grammar.Whitespace (sWhitespace)
import Format.Preserving.YAML.Parser.Token (Token (..))

-- | A tokenizer that capture Float content.
float :: P.Stream s m Char => P.ParsecT s u m Token
float = Float. T.pack <$> nbFloat

-- | A grammar that identify full floating content.
nbFloat :: P.Stream s m Char => P.ParsecT s u m String
nbFloat = (<>) <$> P.option [] strSign <*> float
  where
  float   = P.choice [decimal, number]
  decimal = (<>) <$> nbNsDecimal <*> P.option [] nbNsExp
  number  = (<>) <$> P.many P.digit
                 <*> P.choice [decimal, nbNsExp, P.string "."]

-- | A grammar that identify floating decimal content.
nbNsDecimal :: P.Stream s m Char => P.ParsecT s u m String
nbNsDecimal = (:) <$> P.char '.' <*> P.manyTill P.digit endline
  where
  endline = P.lookAhead cExp <|> bNotStr

-- | A grammar that identify exponent form content.
nbNsExp :: P.Stream s m Char => P.ParsecT s u m String
nbNsExp = (:) <$> (P.char 'e' <|> P.char 'E') <*> number
  where
  number  = (<>) <$> P.option [] strSign <*> P.manyTill P.digit endline
  endline = P.lookAhead bNotStr

-- | A tokenizer that capture Inf content.
--
-- An Inf in YAML represent as `[-+]?\.(inf|Inf|INF)`. Inf is a part of Float
-- in YAML content.
inf :: P.Stream s m Char => P.ParsecT s u m Token
inf = Inf. T.pack <$> nbInf

-- | A grammar that identify Inf content.
nbInf :: P.Stream s m Char => P.ParsecT s u m String
nbInf = (<>) <$> P.option [] strSign <*> (P.lookAhead inf *> collect)
  where
  inf     = (<>) <$> P.string "." <*> strInf
  strInf  = P.string "inf" <|> P.try (P.string "Inf") <|> P.string "INF"
  collect = P.manyTill P.anyChar (P.lookAhead bNotStr)

-- | A tokenizer that capture Nan content.
--
-- A Nan in YAML represent as `\.(nan|NaN|NAN)`. Nan is a part of Float in YAML
-- content.
nan :: P.Stream s m Char => P.ParsecT s u m Token
nan = Nan. T.pack <$> nbNan

-- | A grammar that identify Nan content.
nbNan :: P.Stream s m Char => P.ParsecT s u m String
nbNan = P.char '.' *> P.lookAhead nan *> collect
  where
  nan     = P.string "nan" <|> P.try (P.string "NaN") <|> P.string "NAN"
  collect = P.manyTill P.anyChar (P.lookAhead bNotStr)
