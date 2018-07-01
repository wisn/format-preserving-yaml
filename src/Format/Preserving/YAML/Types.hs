{-# LANGUAGE DeriveGeneric #-}

module Format.Preserving.YAML.Types where

import Data.Text (Text)
import GHC.Generics (Generic)

-- | A YAML native data structure. It stores YAML directives and
-- YAML documents.
--
-- YAML Directive contains YAML version directive and the YAML Tag directive.
--
-- YAML Document contains YAML content like Null, Bool, Int, and so on.
data YAML = YAML Directives Documents deriving (Generic, Show)

-- | A YAML Directive contains single YAML version and distinct YAML Tag.
data Directive
  = Version Text Whitespaces
  | Tag Handle Prefix Whitespaces
  deriving (Generic, Show)

-- | A YAML Document contains YAML content which is arranged by its data type.
data Document = Document Types deriving (Generic, Show)

-- TODO: Implement a proper YAML data type.
data Type
  = Null Text
  | Whitespace Text
  deriving (Show)

type Directives = [Directive]

type Documents = [Document]

type Handle = Text

type Whitespaces = [Text]

type Prefix = Text

type Types = [Type]
