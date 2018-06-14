# Format.Preserving.YAML

A format-preserving YAML parser.

## Progress Note

Currently, `parse` and `format` works pretty good for whitespace, newline, and
comment. There are several bugs occurred though based on the testing. However,
parsing and formatting working just fine so far. It is following the
[YAML 1.2 Spec](http://yaml.org/spec/1.2/spec.html). Any bugs that occurred
will be fixed as soon as possible.

### Production Grammar

The production grammar located at
`src/Format/Preserving/YAML/Parser/Grammar.hs`. Grammar naming convention is
based on the YAML 1.2 Spec
[Grammar Naming Convention](http://yaml.org/spec/1.2/spec.html#id2770517).

### Implementation Validation

I'm reading [Learn YAML in Y Minutes](https://learnxinyminutes.com/docs/yaml/)
to think about the implementation and looked at the YAML 1.2 Spec production
grammar for the best outcome. Making sure that the YAML is valid using online
[YAML Linter](http://www.yamllint.com/) and online
[YAML Parser](http://yaml-online-parser.appspot.com/).

### Usage Example

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid ((<>))
import qualified Data.Text.Lazy.Builder as TLB (toLazyText)
import qualified Data.Text.Lazy.IO as T (putStrLn)

import Format.Preserving.YAML

main :: IO ()
main = do
    let parsed = parse "\t\t\t    \n\n\t# Comment\t   \n"
    putStrLn $ "Parsed:\n" <> show parsed <> "\n"
-- > Parsed:
-- * Right [Tab,Tab,Tab,Space,Space,Space,Space,LineFeed,LineFeed,Tab,
-- * Comment " Comment\t   ",LineFeed]

    case parsed of
        Right yaml -> do
            let formatted = format yaml
            putStrLn $ "Formatted Text:\n" <> show formatted <> "\n"
-- > Formatted Text:
-- * "\t\t\t    \n\n\t# Comment\t   \n"

            putStrLn "YAML:"
            (T.putStrLn. TLB.toLazyText) formatted
-- > YAML:
-- * 			    
-- *
-- *	# Comment	   
-- *
```

## License

Licensed under [The BSD3 License](LICENSE).
