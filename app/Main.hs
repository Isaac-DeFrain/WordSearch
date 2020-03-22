module Main where

import           Data
import           Data.Char     (toUpper)
import           Lib
import           System.IO
import           System.Random

main :: IO ()
main = do
  gen <- newStdGen
  let filledInGrid = fillInBlanks gen grid
      game = makeGame filledInGrid languages
  hSetBuffering stdout NoBuffering
  outputGame game
  playTurn game

playTurn :: Game -> IO ()
playTurn game = do
  putStr "Please enter a word> "
  word <- getLine
  let newGame = playGame game (map toUpper word)
  if completed newGame
  then putStrLn "Congratulations, you win!"
  else do outputGame newGame
          playTurn newGame

completed :: Game -> Bool
completed game = score game == totalWords game
