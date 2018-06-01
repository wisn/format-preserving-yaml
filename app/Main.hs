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

    case parsed of
        Right yaml -> do
            let formatted = format yaml
            putStrLn $ "Formatted Text:\n" <> show formatted <> "\n"

            putStrLn "YAML:"
            (T.putStrLn. TLB.toLazyText) formatted
