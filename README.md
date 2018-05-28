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
`src/Format/Preserving/YAML/Parser.Grammar.hs`. Grammar naming convention is
based on the YAML 1.2 Spec
[Grammar Naming Convention](http://yaml.org/spec/1.2/spec.html#id2770517).

### Usage Example

```haskell
module Main where

import qualified Data.Text as T

import Format.Preserving.YAML

main :: IO ()
main = do
    let origin = "  \n\n# This is a comment\n\t\t# Another one\n   "
        parsed = parse origin

    print parsed
-- > Right (Spaces 2 (EOL 2 (Comment " This is a comment" (EOL 1 (
-- * Tabs 2 (Comment " Another one" (EOL 1 (Spaces 3 EOF))))))))

    case parsed of
        Right yaml -> putStrLn $ T.unpack (format yaml)
-- >   
-- *
-- * # This is a comment
-- * 		# Another one
-- *   
```

## License

Licensed under [The BSD3 License](LICENSE).
