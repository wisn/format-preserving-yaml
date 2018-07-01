{-# LANGUAGE DuplicateRecordFields #-}

module Format.Preserving.YAML.Spec12.Types where

import qualified Data.Map as M (Map)
import qualified Data.Text as T (Text, empty, unpack)

-- | YAML support multi documents in one file. Each document separated by `...`
-- string. If there is parse error, the Documents length will be zero. Please
-- note that single YAML Directive `%YAML` greater than or equal to 2.0 will
-- break the whole parsing process.
data YAML = YAML Documents deriving (Show)

type Documents = [Document]

-- | YAML Document has its own Directive and Node. Version information and tags
-- will be stored in the Directive. YAML Node will be Null if there is no node.
data Document = Document Directive Body deriving (Show)

-- | YAML Directive stores Version information and Tags. By Tags means
-- PrimaryTag, SecondaryTag, and NamedTag followed by the tag name.
data Directive = Directive Version Tags deriving (Show)

{- NOTE: There is a possibility to parse %FOO directive which is should be
  ignored. Need to implement a proper structure for this.
-}

-- | YAML default Version is 1.2 and if there is user defined version, it will
-- stores as Defined followed by the version information.
data Version = Default | Defined T.Text deriving (Show)

-- | YAML Tags stored as a Hashmap and use Handle as its key.
type Tags = M.Map Handle T.Text

-- | YAML PrimaryTag and SecondaryTag doesn't have any names. If there is no
-- SecondaryTag defined by the user, it will use `tag:yaml.org,2000:` by
-- default. Also, if there is no PrimaryTag defined by the user, it will treats
-- as private Tag.
data Handle = PrimaryTag | SecondaryTag | NamedTag T.Text deriving (Show)

data Body = Node Node | SingleMap Nodes deriving (Show)

-- | YAML Node basically built by Scalar, Sequence, and Map.
data Node = Scalar Scalar
          | Sequence Nodes
          | Map Node Nodes deriving (Show)

type Nodes = [Node]

-- | YAML Scalar just like on the Spec but with Comment as the addition.
data Scalar = Bool Bool Format
            | Comment Format
            | Hexadecimal T.Text Format
            | Inf Format
            | Int Int Format
            | NaN Format
            | Null Format
            | Octal T.Text Format
            | Str T.Text Format
            deriving (Eq)

instance Show Scalar where
  show (Bool b _)        = show b
  show (Comment _)       = "Comment"
  show (Hexadecimal h _) = "Hexadecimal " ++ T.unpack h
  show (Inf _)           = "Inf"
  show (Int i _)         = "Int " ++ show i
  show (NaN _)           = "NaN"
  show (Null _)          = "Null"
  show (Octal o _)       = "Octal " ++ T.unpack o
  show (Str s _)         = "Str \"" ++ T.unpack s ++ "\""

(~?>) :: Scalar -> T.Text -> Scalar
(Bool b f)        ~?> w = Bool b (f -!> w)
(Comment f)       ~?> w = Comment (f -!> w)
(Hexadecimal h f) ~?> w = Hexadecimal h (f -!> w)
(Inf f)           ~?> w = Inf (f -!> w)
(Int i f)         ~?> w = Int i (f -!> w)
(NaN f)           ~?> w = NaN (f -!> w)
(Null f)          ~?> w = Null (f -!> w)
(Octal o f)       ~?> w = Octal o (f -!> w)

-- | Format stores all necessary information for the format-preserving
-- purpose. It store the original YAML text as well as the whitespaces.
data Format = Format
            { modified :: Bool
            , original :: T.Text
            , whitespace :: Whitespace
            }
            deriving (Eq)

initialFormat :: Format
initialFormat = Format
              { modified = False
              , original = T.empty
              , whitespace = initialWhitespace
              }

-- | Pass Text to original record in the initial Format.
(=!>) :: T.Text -> Format
(=!>) v = initialFormat { original = v }

-- | Whitespace stores all possible YAML whitespace starting from the left
-- text, middle with List if there is several parts, and the right text if
-- necessary.
data Whitespace = Whitespace
                { after :: T.Text
                , before :: T.Text
                , middle :: [T.Text]
                }
                deriving (Eq)

initialWhitespace = Whitespace
                  { after = T.empty
                  , before = T.empty
                  , middle = []
                  }

(-!>) :: Format -> T.Text -> Format
f -!> w = f { whitespace = initialWhitespace { before = w } }

(-!!>) :: Format -> T.Text -> Format
f -!!> w = f { whitespace = initialWhitespace { after = w } }

(-!!!>) :: Format -> [T.Text] -> Format
f -!!!> w = f { whitespace = initialWhitespace { middle = w } }
