module Format.Preserving.YAML.Parser.Token where

import Data.Text (Text)

data Token
    = CarriageReturn
    | Comment Text
    | LineFeed
    | Space
    | Str Text
    | Tab
    deriving (Eq, Show)

type Tokens = [Token]
