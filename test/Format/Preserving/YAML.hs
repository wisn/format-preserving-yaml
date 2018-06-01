{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Format.Preserving.YAML (format, parse)
import Format.Preserving.YAML.Parser.Token (Token (..))

main :: IO ()
main = hspec $ do
    describe "Format.Preserving.YAML.parse" $ do
        it "returns whitespace production token" $ do
            parse "    \t\t\t "
            `shouldBe`
            (Right [Space, Space, Space, Space, Tab, Tab, Tab, Space])
        it "returns newline production tokens" $ do
            parse "\n\n\n\r\n\r\r"
            `shouldBe`
            (Right [ LineFeed
                   , LineFeed
                   , LineFeed
                   , CarriageReturn
                   , LineFeed
                   , CarriageReturn
                   , CarriageReturn
                   ])
        it "returns comment production tokens" $ do
            parse "# YAML\n"
            `shouldBe`
            (Right [Comment " YAML", LineFeed])
        it "returns comment production tokens (eof)" $ do
            parse "# A comment\n   # Another\tcomment"
            `shouldBe`
            (Right [ Comment " A comment"
                   , LineFeed
                   , Space
                   , Space
                   , Space
                   , Comment " Another\tcomment"
                   ])
        it "returns comment production tokens (carriage-return)" $ do
            parse "\r# YAML\r\n"
            `shouldBe`
            (Right [ CarriageReturn
                   , Comment " YAML"
                   , CarriageReturn
                   , LineFeed
                   ])
    describe "Format.Preserving.YAML.format" $ do
        it "returns whitespace text" $ do
            format [Space, Space, Space, Space, Tab, Tab, Tab, Space]
            `shouldBe`
            "    \t\t\t "
        it "returns newline text" $ do
            format [LineFeed, CarriageReturn, CarriageReturn, LineFeed]
            `shouldBe`
            "\n\r\r\n"
        it "returns comment text" $ do
            format [Comment " YAML", LineFeed]
            `shouldBe`
            "# YAML\n"
        it "returns comment text (eof)" $ do
            format [LineFeed, Comment " YAML\t"]
            `shouldBe`
            "\n# YAML\t"
        it "returns comment text (carriage-return)" $ do
            format [CarriageReturn, Comment " YAML", CarriageReturn]
            `shouldBe`
            "\r# YAML\r"
