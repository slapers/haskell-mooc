module Mastermind () where

import System.Random (randomRIO, mkStdGen, randomRs)
import Data.List (intersperse, partition, sort)

maxTurns = 10
colors = "RGBYW"

fmtGuess :: String -> String -> String
fmtGuess secret guess =
  "| " ++ concat [ " " ++ [g] ++ "  " | g <- guess] ++
  "|  " ++ show corr ++ "   " ++
  "|    " ++ show skewed ++ "   |\n"
  where
    (corr, skewed) = check secret guess

check :: String -> String -> (Int, Int)
check secret guess =
  let
    (corr, miss) = partition (uncurry (==)) (zip secret guess)
  in
  (length corr, uncurry skewed (unzip miss))

skewed :: String -> String -> Int
skewed xs ys = go (sort xs) (sort ys) 0
  where
    go [] _ acc = acc
    go _ [] acc = acc
    go (x:xs) (y:ys) acc
      | x < y = go xs (y:ys) acc
      | x > y = go (x:xs) ys acc
      | otherwise = go xs ys (acc + 1)

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
