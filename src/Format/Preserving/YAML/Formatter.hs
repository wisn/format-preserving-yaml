{-# LANGUAGE OverloadedStrings #-}

module Format.Preserving.YAML.Formatter (format) where

import Data.Monoid ((<>))
import qualified Data.Text.Lazy.Builder as TLB (Builder, fromString, fromText)

import Format.Preserving.YAML.Parser.Token

-- | Format parsed YAML into its original form.
format :: Tokens -> TLB.Builder
format = foldMap token
  where
    token CarriageReturn = "\r"
    token (Comment c)    = "#" <> TLB.fromText c
    token (Float f)      = TLB.fromString (show f)
    token (Int i)        = TLB.fromString (show i)
    token LineFeed       = "\n"
    token Space          = " "
    token (Str s)        = TLB.fromText s
    token Tab            = "\t"
