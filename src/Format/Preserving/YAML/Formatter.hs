{-# LANGUAGE OverloadedStrings #-}

module Format.Preserving.YAML.Formatter where

import Data.Monoid ((<>))
import qualified Data.Text.Lazy.Builder as TLB (Builder, fromString, fromText)

-- import Format.Preserving.YAML.Parser.Token
--
-- -- | Format parsed YAML into its original form.
-- format :: Tokens -> TLB.Builder
-- format = foldMap token
--   where
--     token (Bool _ b)      = TLB.fromText b
--     token CarriageReturn  = "\r"
--     token (Comment c)     = "#" <> TLB.fromText c
--     token (Hexadecimal h) = "0x" <> TLB.fromText h
--     token (Inf i)         = TLB.fromText i
--     token (Int i)         = TLB.fromString (show i)
--     token LineFeed        = "\n"
--     token (Nan n)         = "." <> TLB.fromText n
--     token (Null n)        = TLB.fromText n
--     token (Octal o)       = "0o" <> TLB.fromText o
--     token Space           = " "
--     token (Str s)         = TLB.fromText s
--     token Tab             = "\t"
