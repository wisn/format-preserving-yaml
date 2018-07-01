module Format.Preserving.YAML.Parsec.Utils where

import qualified Text.Parsec as P (Stream, ParsecT, lookAhead, manyTill)

(>?) :: P.Stream s m t => P.ParsecT s u m a -> P.ParsecT s u m a
(>?) = P.lookAhead

(++>) :: P.Stream s m t
      => P.ParsecT s u m a -> P.ParsecT s u m end -> P.ParsecT s u m [a]
p ++> end = P.manyTill p end
