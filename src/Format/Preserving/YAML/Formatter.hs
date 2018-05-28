module Format.Preserving.YAML.Formatter (format) where

import qualified Data.Text as T

import Format.Preserving.YAML.Parser.Token

class Format a where
    format :: a -> T.Text

-- | Format instance that retrieve a YAML Token then transform it to Text.
instance Format Token where
    format = formatToken

formatToken :: Token -> T.Text
formatToken (Comment c t)
    = T.concat [T.concat [T.singleton '#', c], formatToken t]
formatToken EOF
    = T.empty
formatToken (EOL n t)
    = T.concat [T.pack $ replicate n '\n', formatToken t]
formatToken (Tabs n t)
    = T.concat [T.pack $ replicate n '\t', formatToken t]
formatToken (Spaces n t)
    = T.concat [T.pack $ replicate n ' ', formatToken t]
