{-# LANGUAGE FlexibleContexts #-}

module Format.Preserving.YAML.Parser.Grammar.Scalar.Integer where

import Control.Applicative ((<|>))
import qualified Data.Text as T (pack)
import qualified Text.Parsec as P

import Format.Preserving.YAML.Parser.Grammar.Common (bNotStr, cSign)
import Format.Preserving.YAML.Parser.Grammar.Newline (bNewline)
import Format.Preserving.YAML.Parser.Token (Token (..))

-- | A tokenizer that capture Int content.
--
-- An integer in YAML represent using the [0-9] character and sometimes begin
-- with a (-|+) sign. The accepted integer are `1234567890`, `-1`, `+2`, and
-- so on. Note that `1234    s` considered as Str. Mapping between Int to
-- something else is posible. So, `1234:...` also acceptable.
int :: P.Stream s m Char => P.ParsecT s u m Token
int = Int. read <$> nbInt

-- | A grammar that identify and collect Int content.
nbInt :: P.Stream s m Char => P.ParsecT s u m String
nbInt = (:) <$> (cSign <|> P.digit)
            <*> P.manyTill P.digit (P.lookAhead bNotStr)

-- | A tokenizer that capture Octal content.
--
-- An octal in YAML represent as `0o[0-7]+`. As an example, `0o123` will be
-- captured as Octal "123" and `0o1238` will be captured as Str "0o1238".
octal :: P.Stream s m Char => P.ParsecT s u m Token
octal = Octal. T.pack <$> nbOctal

-- | A grammar that identify and collect Octal content.
nbOctal :: P.Stream s m Char => P.ParsecT s u m String
nbOctal = P.string "0o" *> P.manyTill octal bNotStr
  where
  octal = P.oneOf ['0'..'7']

-- | A tokenizer that capture Hexadecimal content.
--
-- A hexadecimal in YAML represent as `0o([0-9]|[A-F])+`. As an example,
-- `0x123F` will be captured as Hexadecimal "123F" and `0x123G` will be
-- captured as Str "0x123G".
hexadecimal :: P.Stream s m Char => P.ParsecT s u m Token
hexadecimal = Hexadecimal. T.pack <$> nbHexadecimal

-- | A grammar that identify and collect Hexadecimal content.
nbHexadecimal :: P.Stream s m Char => P.ParsecT s u m String
nbHexadecimal = P.string "0x" *> P.manyTill hexadecimal bNotStr
  where
  hexadecimal = P.digit <|> P.oneOf ['a'..'f'] <|> P.oneOf ['A'..'F']
