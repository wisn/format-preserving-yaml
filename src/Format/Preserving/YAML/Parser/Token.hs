module Format.Preserving.YAML.Parser.Token where

import Data.Text (Text)

data Token
    = Bool Bool Text
    | CarriageReturn
    | Comment Text
    | Float Text
    | Hexadecimal Text
    | Inf Text
    | Int Int
    | LineFeed
    | Nan Text
    | Null Text
    | Octal Text
    | Space
    | Str Text
    | Tab
    deriving (Eq, Show)

type Tokens = [Token]
