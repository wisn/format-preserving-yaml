{-# LANGUAGE FlexibleContexts #-}

module Format.Preserving.YAML.Spec12.Grammar.Scalar (scalar) where

import Prelude hiding (null)

import Control.Applicative ((<|>))
import Data.Monoid ((<>))
import Data.Text (Text)
import Format.Preserving.YAML.Parsec.Utils ((++>))
import Format.Preserving.YAML.Spec12.Grammar.Common
  ( eos
  , newline
  , printable
  , terminator
  , whitespace
  )
import Format.Preserving.YAML.Spec12.Types (Scalar (..), (=!>), (~?>))
import qualified Data.Text as T (pack)
import qualified Text.Parsec as P
  ( Stream
  , Parsec
  , ParsecT
  , char
  , choice
  , digit
  , many
  , oneOf
  , option
  , string
  , try
  )

-- | A grammar for parsing a YAML Scalar that also captures whitespace
-- & newline.
scalar :: P.Parsec Text () Scalar
scalar = do
  spaces <- P.many (whitespace <|> newline)
  scalar <- P.choice [ comment
                     , null
                     , bool
                     , P.try int
                     , P.try octal
                     , hexadecimal
                     , P.try inf
                     , nan
                     ]

  return $ scalar ~?> (T.pack spaces)

-- | A grammar for parsing a YAML Comment until newline or EOF.
comment :: P.Parsec Text () Scalar
comment = do
  P.char '#'
  comment <- printable ++> terminator
  let format = (=!>) (T.pack comment)

  return $ Comment format


-- | A grammar for parsing a YAML Null such as null, Null, and NULL based on
-- the YAML 1.2 Spec Core Schema.
null :: P.Parsec Text () Scalar
null = do
  null <- P.choice [P.string "null", P.try (P.string "Null"), P.string "NULL"]
  let format = (=!>) (T.pack null)

  return $ Null format

-- | A grammar for parsing a YAML Bool such as true, True, TRUE, false, False,
-- and FALSE based on the YAML 1.2 Spec Core Schema.
bool :: P.Parsec Text () Scalar
bool = true <|> false

true :: P.Parsec Text () Scalar
true = do
  true <- P.choice [P.string "true", P.try (P.string "True"), P.string "TRUE"]
  let format = (=!>) (T.pack true)

  return $ Bool True format

false :: P.Parsec Text () Scalar
false = do
  false <- P.choice [ P.string "false"
                    , P.try (P.string "False")
                    , P.string "FALSE"
                    ]
  let format = (=!>) (T.pack false)

  return $ Bool False format

sign :: P.Stream s m Char => P.ParsecT s u m String
sign = P.string "-" <|> P.string "+"

-- | A grammar for parsing a YAML Int. The accepted string is `[-+]? [0-9]+`.
int :: P.Parsec Text () Scalar
int = do
  int <- (<>) <$> P.option [] sign <*> P.digit ++> eos
  let format = (=!>) (T.pack int)
      parsed = if (length int > 0) && (head int == '+') then tail int else int

  return $ Int (read parsed) format

-- | A grammar for parsing a YAML (unsigned) Octal which is part of Int.
octal :: P.Parsec Text () Scalar
octal = do
  P.string "0o"
  octal <- P.oneOf ['0'..'7'] ++> eos
  let packed = T.pack octal
      format = (=!>) packed

  return $ Octal packed format

-- | A grammar for parsing a YAML (unsigned) Hexadecimal which is part of Int.
hexadecimal :: P.Parsec Text () Scalar
hexadecimal = do
  P.string "0x"
  hexadecimal <- P.many $ P.digit <|> P.oneOf (['A'..'F'] ++ ['a'..'f'])
  let packed = T.pack hexadecimal
      format = (=!>) packed

  return $ Hexadecimal packed format

-- | A grammar for parsing a YAML Inf which is part of Float.
inf :: P.Parsec Text () Scalar
inf = do
  inf <- (<>) <$> P.option [] sign
              <*> P.choice [ P.try (P.string ".inf")
                           , P.try (P.string ".Inf")
                           , P.string ".INF"
                           ]
  let format = (=!>) (T.pack inf)

  return $ Inf format

-- | A grammar for parsing a YAML Nan which is part of Float.
nan :: P.Parsec Text () Scalar
nan = do
  nan <- P.choice [ P.try (P.string ".nan")
                  , P.try (P.string ".NaN")
                  , P.string ".NAN"
                  ]
  let format = (=!>) (T.pack nan)

  return $ NaN format
