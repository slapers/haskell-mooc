module Mastermind () where

import System.Random (randomRIO, mkStdGen, randomRs)
import Data.List (intersperse)

maxTurns = 10
colors = "RGBYW"

fmtGuess :: String -> String -> String
fmtGuess secret guess =
  "| " ++ concat [ " " ++ [g] ++ "  " | g <- guess] ++
  "|  " ++ show corr ++ "   " ++
  "|    " ++ show skewed ++ "   |\n"
  where
    corr = length . filter id $ uncurry (==) <$> zip secret guess
    skewed = (length . filter id $ ($ guess) . elem <$> secret) - corr

paint :: String -> [String] -> IO ()
paint secret guesses =
  putStr header >> putStr guessesFmt >> putStr footer
  where
    guessesFmt = concat $ fmtGuess secret <$> reverse guesses
    header = "+-----------------+------+--------+\n" ++
             "| [?] [?] [?] [?] | CORR | SKEWED |\n" ++
             "+-----------------+------+--------+\n"
    footer = "+---------------------------------+\n"

askNext :: [String] -> IO [String]
askNext previous =
  askGuess >> consGuess <$> getLine
  where
    askGuess = putStr $ concat ["Your guess : (", intersperse ',' colors , ") :"]
    consGuess x = take 4 x : previous

randomColors :: IO String
randomColors = take 4 . map (colors !!) <$> generator
  where
    generator = randomRs (0, length colors - 1) . mkStdGen <$> randomRIO (0, 99)

turn :: String -> [String] -> IO ()
turn secret guesses
  | null guesses = paintAttempts >> nextAttempt
  | length guesses > maxTurns = putStrLn ("sorry .. secret was " ++ secret)
  | head guesses == secret = putStrLn "congrats.. secret found"
  | otherwise = paintAttempts >> nextAttempt
  where
    paintAttempts = paint secret guesses
    nextAttempt = turn secret =<< askNext guesses

main :: IO ()
main =
  do putStrLn "Guess the 4 colors"
     secret <- randomColors
     turn secret []
