{-# LANGUAGE FlexibleContexts #-}

module Format.Preserving.YAML.Spec12.Grammar.Directive where

import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Format.Preserving.YAML.Parsec.Utils ((++>))
import Format.Preserving.YAML.Spec12.Grammar.Common
  ( printable
  , terminator
  , whitespace
  )
import Format.Preserving.YAML.Spec12.Types
  ( Directive (..)
  , Handle (..)
  , Tag
  , Version (..)
  , (=!>)
  , (~|||>)
  , initialFormat
  )
import qualified Text.Parsec as P
 ( Parsec
 , char
 , choice
 , digit
 , letter
 , many1
 , string
 , try
 )

-- | A grammar for parsing a YAML Directive (Version, Tags) and an ignored
-- none-reserved Directive.
directive :: P.Parsec Text () Directive
directive = do
  directive <- P.choice [P.try version, P.try tag, ignored]

  return $ directive

-- | A grammar for parsing a YAML Version Directive.
version :: P.Parsec Text () Directive
version = do
  P.string "%YAML"
  spaces <- P.many1 whitespace
  let digits = P.many1 P.digit
  version <- (<>) <$> digits <*> ((:) <$> P.char '.' <*> digits)

  let yaml     = pack "%YAML"
      spaces'  = pack spaces
      version' = pack version
      format   = (=!>) (yaml <> spaces' <> version')
      parsed   = if version == "1.2" then Default else (UserDefined version')

  return $ (Version parsed format) ~|||> [spaces']

-- | A grammar for parsing a YAML Tag Directive (primary tag, secondary tag,
-- and named tag).
tag :: P.Parsec Text () Directive
tag = do
  P.string "%TAG"
  spaces <- P.many1 whitespace
  tag    <- P.choice [P.try primaryTag, P.try secondaryTag, namedTag]

  let tag'      = pack "%TAG"
      spaces'   = [pack spaces, pack (fst tag)]
      format    = (=!>) (tag' <> head spaces' <> directive)
      directive =
        case (snd tag) of
          (PrimaryTag, prefix)
            -> pack "!" <> last spaces' <> prefix
          (SecondaryTag, prefix)
            -> pack "!!" <> last spaces' <> prefix
          (NamedTag name, prefix)
            -> pack "!" <> name <> pack "!" <> last spaces' <> prefix

  return $ (Tag (snd tag) format) ~|||> spaces'

-- | A grammar for parsing a YAML none-reserved Directive.
ignored :: P.Parsec Text () Directive
ignored = do
  P.char '%'
  directive <- printable ++> terminator

  let packed = pack directive
      format = (=!>) (pack "%" <> packed)

  return $ Ignored packed format

primaryTag :: P.Parsec Text () (String, Tag)
primaryTag = do
  P.string "!"
  spaces <- P.many1 whitespace
  prefix <- printable ++> terminator

  return (spaces, (PrimaryTag, pack prefix))

secondaryTag :: P.Parsec Text () (String, Tag)
secondaryTag = do
  P.string "!!"
  spaces <- P.many1 whitespace
  prefix <- printable ++> terminator

  return (spaces, (SecondaryTag, pack prefix))

namedTag :: P.Parsec Text () (String, Tag)
namedTag = do
  P.string "!"
  name <- P.many1 P.letter
  P.string "!"
  spaces <- P.many1 whitespace
  prefix <- printable ++> terminator

  return (spaces, (NamedTag (pack name), pack prefix))
