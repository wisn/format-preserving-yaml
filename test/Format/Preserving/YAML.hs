import Test.Hspec

import qualified Data.Text as T

import Format.Preserving.YAML
import Format.Preserving.YAML.Parser.Token

main :: IO ()
main = hspec $ do
    describe "Format.Preserving.YAML.parse" $ do
        it "returns whitespace productions token" $ do
            parse "    \t\t\t "
            `shouldBe`
            (Right (Spaces 4 (Tabs 3 (Spaces 1 EOF))))
        it "returns newline productions token" $ do
            parse "\n\n\n\r\r\r\r" -- Complete test will be added later
            `shouldBe`
            (Right (EOL 3 (EOL 4 EOF)))
        it "returns comment production token (any)" $ do
            parse "# YAML\n"
            `shouldBe`
            (Right (Comment (T.pack " YAML") (EOL 1 EOF)))
        it "returns comment productions token (eof)" $ do
            -- BUG: not catching EOF after a comment
            parse "# A comment\n   # Another\tcomment"
            `shouldBe`
            (Right (Comment (T.pack " A comment")
            (EOL 1 (Spaces 3 (Comment (T.pack " Another\tcomment") EOF)))))
    describe "Format.Preserving.YAML.format" $ do
        it "returns whitespaces text" $ do
            format (Spaces 4 (Tabs 3 (Spaces 1 EOF)))
            `shouldBe`
            (T.pack "    \t\t\t ")
        it "returns newline text" $ do
            -- BUG: Newline not distinguished
            format (EOL 3 (EOL 4 EOF))
            `shouldBe`
            (T.pack "\n\n\n\r\r\r\r")
        it "returns comment text" $ do
            format (Comment (T.pack " YAML") (EOL 1 EOF))
            `shouldBe`
            (T.pack "# YAML\n")
