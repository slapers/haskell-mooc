module HaskellMoocParsecWeek4 where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import Data.List (intercalate)


-- makeTokenParser creates a `GenTokenParser` record that contains
-- the lexical parsers that are defined in the language
-- emptyDef is a very basic language definition without comments, reserved words etc
-- http://hackage.haskell.org/package/parsec-3.1.13.0/docs/src/Text.Parsec.Language.html#emptyDef
lexer = P.makeTokenParser emptyDef

identifier = P.identifier lexer
angles = P.angles lexer

parseTag1 :: Parser String
parseTag1 = do
  char '<'
  x <- identifier
  char '>'
  return x

parseTag2 :: Parser String
parseTag2 = angles identifier

parseTag3 :: Parser String
parseTag3 = do
  string "<>"
  x <- identifier
  string "<>"
  return x

letterOrDigit :: Parser String
letterOrDigit = show <$> (letter <|> digit)

bagBogConsumes :: Parser String
bagBogConsumes = string "bag" <|> string "bog"

bagBogTry :: Parser String
bagBogTry = try (string "bag") <|> string "bog"

test :: String -> [String] -> Parser String -> IO ()
test desc xs parser = do
  putStrLn desc >> putStrLn ['-'|d<-desc]
  let outs = show . parse parser "" <$> xs
  putStr $ intercalate "\n" [ x ++ " -> " ++ o | (x,o) <- zip xs outs]
  putStrLn "\n"

main :: IO ()
main = do
  test "parseTag1" ["<me>", "< me >"] parseTag1
  test "parseTag2" ["<me>", "< me >"] parseTag2
  test "parseTag2" ["<me>", "<>me<>"] parseTag3
  test "letterOrDigit" ["a", "11"] letterOrDigit
  test "bagBogConsumes" ["bag", "bog"] bagBogConsumes
  test "bagBogTry" ["bag", "bog"] bagBogTry
