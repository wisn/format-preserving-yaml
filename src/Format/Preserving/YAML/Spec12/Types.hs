{-# LANGUAGE DuplicateRecordFields #-}

module Format.Preserving.YAML.Spec12.Types where

import Data.Scientific (Scientific)
import qualified Data.Text as T (Text, empty, unpack)

-- | YAML support multi documents in one file. Each document separated by `...`
-- string. If there is parse error, the Documents length will be zero. Please
-- note that single YAML Directive `%YAML` greater than or equal to 2.0 will
-- breaks the whole parsing process.
data YAML = YAML Documents deriving (Show)

type Documents = [Document]

-- | YAML Document has its own Directive and Node. Version information and tags
-- will be stored in the Directive. YAML Node will be Null if there is no node.
data Document = Document Directives Node deriving (Show)

type Directives = [Directive]

-- | YAML Directive stores Version information and Tags. By Tags means
-- PrimaryTag, SecondaryTag, and NamedTag followed by the tag name. YAML also
-- parse none-reserved Directive identified by the Ignored type.
data Directive = Ignored T.Text Format
               | Tag Tag Format
               | Version Version Format
               deriving (Eq)

instance Show Directive where
  show (Ignored c _) = "Ignored \"" ++ T.unpack c ++ "\""
  show (Tag c _)     = show c
  show (Version c _) = "Version " ++ show c

(~|||>) :: Directive -> [T.Text] -> Directive
(Tag c f)     ~|||> w = Tag c (f -!!!> w)
(Version c f) ~|||> w = Version c (f -!!!> w)
(Ignored c f) ~|||> w = Ignored c (f -!!!> w)

-- | YAML default Version is 1.2 and if there is user defined version, it will
-- stores as Defined followed by the version information.
data Version = Default | UserDefined T.Text deriving (Eq, Show)

-- | YAML Tags stored as a pair of Handle and Text. Handle acts as the key.
type Tag = (Handle, T.Text)

-- | YAML PrimaryTag and SecondaryTag doesn't have any names. If there is no
-- SecondaryTag defined by the user, it will use `tag:yaml.org,2000:` by
-- default. Also, if there is no PrimaryTag defined by the user, it will treats
-- as a private Tag.
data Handle = PrimaryTag | SecondaryTag | NamedTag T.Text deriving (Eq, Show)

-- | YAML Node basically built by Scalar, Sequence, and Map.
data Node = Scalar Scalar
          | Sequence Sequence
          | Map Map
          | SingleMap Maps
          deriving (Eq, Show)

type Sequence = Nodes

-- | YAML Map separated to the Node structure since YAML could contains List of
-- Map.
type Map = (Node, Nodes)

type Nodes = [Node]

type Maps = [Map]

-- | YAML Scalar just like on the Spec but with Comment as the addition.
data Scalar = Anchor T.Text T.Text Format -- NOTE: Accepts map, seq, and scalar
            | Alias T.Text Format
            | Bool Bool Format
            | Comment Format
            | DoubleQuoted T.Text Format
            | Float Scientific Format
            | Hexadecimal T.Text Format
            | Inf Format
            | Int Int Format
            | NaN Format
            | Null Format
            | Octal T.Text Format
            | SingleQuoted T.Text Format
            | Str T.Text Format
            deriving (Eq)

instance Show Scalar where
  -- show (Anchor k v _)     = "Anchor \"" ++ T.unpack k ++ "\" "
  --                               ++ "\"" ++ T.unpack v ++ "\""
  show (Alias c _)        = "Alias \"" ++ T.unpack c ++ "\""
  show (Bool c _)         = show c
  show (Comment _)        = "Comment"
  show (DoubleQuoted c _) = "Str \"" ++ T.unpack c ++ "\""
  show (Float c _)        = "Float " ++ show c
  show (Hexadecimal c _)  = "Hexadecimal " ++ T.unpack c
  show (Inf _)            = "Inf"
  show (Int c _)          = "Int " ++ show c
  show (NaN _)            = "NaN"
  show (Null _)           = "Null"
  show (Octal c _)        = "Octal " ++ T.unpack c
  show (SingleQuoted c _) = "Str \"" ++ T.unpack c ++ "\""
  show (Str c _)          = "Str \"" ++ T.unpack c ++ "\""

-- | Replace `before` Whitespace Format.
(~?>) :: Scalar -> T.Text -> Scalar
(Anchor k v f)     ~?> w = Anchor k v (f -!> w)
(Alias c f)        ~?> w = Alias c (f -!> w)
(Bool c f)         ~?> w = Bool c (f -!> w)
(Comment f)        ~?> w = Comment (f -!> w)
(DoubleQuoted c f) ~?> w = DoubleQuoted c (f -!> w)
(Float c f)        ~?> w = Float c (f -!> w)
(Hexadecimal c f)  ~?> w = Hexadecimal c (f -!> w)
(Inf f)            ~?> w = Inf (f -!> w)
(Int c f)          ~?> w = Int c (f -!> w)
(NaN f)            ~?> w = NaN (f -!> w)
(Null f)           ~?> w = Null (f -!> w)
(Octal c f)        ~?> w = Octal c (f -!> w)
(SingleQuoted c f) ~?> w = SingleQuoted c (f -!> w)

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
f@(Format { whitespace = w }) -!> b = f { whitespace = w { before = b } }

(-!!>) :: Format -> T.Text -> Format
f@(Format { whitespace = w }) -!!> a = f { whitespace = w { after = a } }

(-!!!>) :: Format -> [T.Text] -> Format
f@(Format { whitespace = w }) -!!!> m = f { whitespace = w { middle = m } }
