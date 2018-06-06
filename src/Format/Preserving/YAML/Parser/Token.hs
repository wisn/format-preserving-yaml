module Format.Preserving.YAML.Parser.Token where

import Data.Text (Text)

data Token
    = Bool Bool
    | CarriageReturn
    | Comment Text
    | Float Float
    | Int Int
    | LineFeed
    | Space
    | Seq Tokens
    | Str Text
    | Tab
    deriving (Eq, Show)

type Tokens = [Token]
