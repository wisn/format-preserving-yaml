{-# LANGUAGE FlexibleContexts #-}

module Format.Preserving.YAML.Spec12.Grammar.Scalar (scalar) where

import Prelude hiding (exp, exponent, null)

import Control.Applicative ((<|>))
import Data.Monoid ((<>))
import Data.Scientific (Scientific)
import Data.Text (Text, pack)
import Format.Preserving.YAML.Parsec.Utils ((>?), (++>))
import Format.Preserving.YAML.Spec12.Grammar.Common
  ( eos
  , newline
  , printable
  , terminator
  , whitespace
  )
import Format.Preserving.YAML.Spec12.Types
  ( Node (..)
  , Scalar (..)
  , (=!>)
  , (~?>)
  )
import qualified Text.Parsec as P
  ( Stream
  , Parsec
  , ParsecT
  , alphaNum
  , char
  , choice
  , digit
  , many
  , many1
  , oneOf
  , option
  , string
  , try
  )

-- | A grammar for parsing a YAML Scalar that also captures whitespace
-- & newline.
scalar :: P.Parsec Text () Node
scalar = do
  spaces <- P.many (whitespace <|> newline)
  scalar <- P.choice [ comment
                     , P.try null
                     , P.try bool
                     , P.try int
                     , P.try octal
                     , P.try hexadecimal
                     , P.try float
                     , P.try inf
                     , P.try nan
                     , singleQuoted
                     , doubleQuoted
                     , alias
                     ]

  return $ Scalar $ scalar ~?> (pack spaces)

-- | A grammar for parsing a YAML Comment until newline or EOF.
comment :: P.Parsec Text () Scalar
comment = do
  P.char '#'
  comment <- printable ++> terminator
  let format = (=!>) (pack comment)

  return $ Comment format

-- | A grammar for parsing a YAML Null such as null, Null, and NULL based on
-- the YAML 1.2 Spec Core Schema.
null :: P.Parsec Text () Scalar
null = do
  null <- P.choice [P.string "null", P.try (P.string "Null"), P.string "NULL"]
  let format = (=!>) (pack null)

  return $ Null format

-- | A grammar for parsing a YAML Bool such as true, True, TRUE, false, False,
-- and FALSE based on the YAML 1.2 Spec Core Schema.
bool :: P.Parsec Text () Scalar
bool = true <|> false

true :: P.Parsec Text () Scalar
true = do
  true <- P.choice [P.string "true", P.try (P.string "True"), P.string "TRUE"]
  let format = (=!>) (pack true)

  return $ Bool True format

false :: P.Parsec Text () Scalar
false = do
  false <- P.choice [ P.string "false"
                    , P.try (P.string "False")
                    , P.string "FALSE"
                    ]
  let format = (=!>) (pack false)

  return $ Bool False format

sign :: P.Stream s m Char => P.ParsecT s u m String
sign = P.string "-" <|> P.string "+"

-- | A grammar for parsing a YAML Int. The accepted string is `[-+]? [0-9]+`.
int :: P.Parsec Text () Scalar
int = do
  int <- (<>) <$> P.option [] sign <*> P.digit ++> eos
  let format = (=!>) (pack int)
      parsed = if (length int > 0) && (head int == '+') then tail int else int

  return $ Int (read parsed :: Int) format

-- | A grammar for parsing a YAML (unsigned) Octal which is part of Int.
octal :: P.Parsec Text () Scalar
octal = do
  P.string "0o"
  octal <- P.oneOf ['0'..'7'] ++> eos
  let packed = pack octal
      format = (=!>) packed

  return $ Octal packed format

-- | A grammar for parsing a YAML (unsigned) Hexadecimal which is part of Int.
hexadecimal :: P.Parsec Text () Scalar
hexadecimal = do
  P.string "0x"
  hexadecimal <- P.many $ P.digit <|> P.oneOf (['A'..'F'] ++ ['a'..'f'])
  let packed = pack hexadecimal
      format = (=!>) packed

  return $ Hexadecimal packed format

-- | A grammar for parsing a YAML Float.
float :: P.Parsec Text () Scalar
float = do
  let digits   = P.many1 P.digit
      decimal  = (<>) <$> P.string "." <*> digits
      decimal' = (<>) <$> P.string "." <*> P.many P.digit
      exp      = P.string "e" <|> P.string "E"
      exponent = (<>) <$> exp <*> ((<>) <$> P.option [] sign <*> digits)
      number   = (<>) <$> digits <*> P.option [] decimal'

  float <- (<>) <$> P.option [] sign
                <*> ((<>) <$> (decimal <|> number) <*> P.option [] exponent)

  let format = (=!>) (pack float)
      float' = if head float == '.' then ('0' : float) else float
      parsed = if last float' == '.' then (float' ++ "0") else float'

  return $ Float (read parsed :: Scientific) format

-- | A grammar for parsing a YAML Inf which is part of Float.
inf :: P.Parsec Text () Scalar
inf = do
  inf <- (<>) <$> P.option [] sign
              <*> P.choice [ P.try (P.string ".inf")
                           , P.try (P.string ".Inf")
                           , P.string ".INF"
                           ]
  let format = (=!>) (pack inf)

  return $ Inf format

-- | A grammar for parsing a YAML Nan which is part of Float.
nan :: P.Parsec Text () Scalar
nan = do
  nan <- P.choice [ P.try (P.string ".nan")
                  , P.try (P.string ".NaN")
                  , P.string ".NAN"
                  ]
  let format = (=!>) (pack nan)

  return $ NaN format

-- | A grammar for parsing a YAML Str with a single quote indicator.
singleQuoted :: P.Parsec Text () Scalar
singleQuoted = do
  P.char '\''
  str <- printable ++> P.char '\''
  -- TODO: Skip escaped character.

  let packed = pack str
      format = (=!>) packed

  return $ SingleQuoted packed format

-- | A grammar for parsing a YAML Str with a double quote indicator.
doubleQuoted :: P.Parsec Text () Scalar
doubleQuoted = do
  P.char '\"'
  str <- printable ++> P.char '\"'
  -- TODO: Skip escaped character.

  let packed = pack str
      format = (=!>) packed

  return $ DoubleQuoted packed format

-- | A grammar for parsing a YAML Alias of an Anchor.
alias :: P.Parsec Text () Scalar
alias = do
  P.char '*'
  alias <- P.many1 (P.alphaNum <|> P.oneOf "-_")

  let packed = pack alias
      format = (=!>) packed

  return $ Alias packed format

-- NOTE: Accepts map, seq, and scalar.
anchor :: P.Parsec Text () Scalar
anchor = do
  P.char '&'
  k <- P.many1 (P.alphaNum <|> P.oneOf "-_")
  spaces <- P.many1 whitespace
  v <- scalar

  let key = pack k
      format = (=!>) (key <> pack spaces)

  return $ Anchor key key format -- NOTE: Value could be map, seq, or scalar.
