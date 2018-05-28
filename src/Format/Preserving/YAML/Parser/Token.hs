module Format.Preserving.YAML.Parser.Token where

import qualified Data.Text as T

data Token
    = Comment T.Text Token
    | EOF
    | EOL Int Token
    | Spaces Int Token
    | Tabs Int Token
    deriving (Eq, Show)
