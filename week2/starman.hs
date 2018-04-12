-- starman.hs
-- Jeremy Singer
-- based on a Functional Programming
-- exercise from Glasgow,
-- (inherited from John O'Donnell)
-- Stefan: adding random and figuring out do, fmap and bind
module Starman where

import System.Random (randomRIO)

turn :: String -> String -> Int -> IO ()
turn word display n
  | n == 0 = putStrLn "You lose"
  | word == display = putStrLn "You win!"
  | otherwise =
      uncurry (turn word) =<< (check word display n <$> mkGuess display n)

mkGuess :: String -> Int -> IO Char
mkGuess display n =
  putStr output >> head <$> getLine
  where
    stars = replicate n '*'
    output = concat [display, "  ", stars, "\n  Enter your guess: "]

check :: String -> String -> Int -> Char -> (String, Int)
check word display n c =
  let
    n' = if c `elem` word then n else n - 1
    display' = [if x==c then c else y | (x,y) <- zip word display]
  in (display', n')

randomWord :: IO String
randomWord = (!!) ["haskell", "erlang", "elixir", "elm", "purescript"] <$> randomRIO (0, 4)

starman :: Int -> IO ()
starman n = do
  word <- randomWord
  turn word ['-' | _ <- word] n
