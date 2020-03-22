module Test
    ( filtered
    , mapped
    , mappedAndFiltered
    , coord
    , coords
    , coordsInf
    , og
    ) where

import           Control.Monad (guard)
import           Data
import           Lib

--data Cell = Cell (Integer, Integer) Char deriving (Eq, Ord)
--instance Show Cell where
--  show (Cell (i, j) c) = show (i, j) ++ " " ++ [c]

div3 :: Int -> Bool
div3 n = n `mod` 3 == 0

-- Monad notation i.e. do
doMapped = do
  i <- [0..]
  return (i * 3)

doFiltered = do
  i <- [0..]
  guard $ div3 i
  return i

doMappedAndFiltered = do
  i <- [0..]
  guard $ div3 i
  return (i + 1)

-- list comprehension
mapped = [i * 3 | i <- [0..]]
filtered = [i | i <- [0..], div3 i]
mappedAndFiltered = [i + 1 | i <- [0..], div3 i]

--coordinate grid associated to letter grid
coord :: Grid Char -> [[(Int, Int)]]
coord (l : ls) =
  let rowNum = length ls
      colNum = length l - 1
  in do
    row <- [0..rowNum]
    return $ do
      col <- [0..colNum]
      return (row, col)

coords = coord grid
coordsInf = zipWith zip rowsInf colsInf

--gridWithCoords :: Grid Char -> Grid Cell
--gridWithCoords = zipGrid coordsInf

--printEach :: Show a => [[a]] -> IO ()
--printEach (l : ls) = print l ; (printEach ls)
--printEach _ = return ()

cols :: Grid a -> [[Int]]
cols grid =
  let h = height grid
  in repeatN h $ take h [0..]

rows :: Grid a -> [[Int]]
rows grid =
  let w = width grid
  in map (repeatN w) (take w [0..])

colsInf = map repeat [0..]
rowsInf = repeat [0..]

og :: Show a => [a] -> IO ()
og = putStrLn . unlines . map show

repeatN :: Int -> a -> [a]
repeatN n a = if (n < 2) then [a]
              else [a] ++ repeatN (n - 1) a
