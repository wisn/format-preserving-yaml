module Main where

import qualified Data.Text as T

import Format.Preserving.YAML

main :: IO ()
main = do
    let yaml = parse "\t\t\t    \n\n\t# Comment\t   \n"
    print yaml

    case yaml of
        Right x -> do
            let f = format x
            print f
            putStrLn $ T.unpack f
