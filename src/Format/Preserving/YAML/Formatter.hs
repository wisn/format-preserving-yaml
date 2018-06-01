{-# LANGUAGE OverloadedStrings #-}

module Format.Preserving.YAML.Formatter (format) where

import Data.Monoid ((<>))
import qualified Data.Text.Lazy.Builder as TLB (Builder, fromText)

import Format.Preserving.YAML.Parser.Token

-- | Format parsed YAML into its original form.
format :: Tokens -> TLB.Builder
format = foldMap token
  where
    token CarriageReturn = "\r"
    token (Comment c)    = "#" <> TLB.fromText c
    token LineFeed       = "\n"
    token Space          = " "
    token Tab            = "\t"
