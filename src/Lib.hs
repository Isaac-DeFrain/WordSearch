module Lib
    ( outputGrid
    , formatGrid
    , formatGame
    , outputGame
    , findWord
    , findWords
    , findWordInLine
    , getLines
    , skew
    , diagonalize
    , height
    , width
    , gridWithCoords
    , zipCell
    , zipGrid
    , zipGridWith
    , cell2char
    , cells2string
    , cells2strings
    , findWordInCellLinePrefix
    , Grid
    , Cell(Cell, Indent)
    , Cells
    , Game(gameGrid, gameWords)
    , makeGame
    , totalWords
    , score
    , playGame
    , makeRandomGrid
    , fill
    , fillInBlanks
    ) where

import           Data.Char     (toLower)
import           Data.List     (isInfixOf, transpose)
import qualified Data.Map      as Map
import           Data.Maybe    (catMaybes, listToMaybe)
import           System.Random (RandomGen, randomRs, split)

type Grid a = [[a]]
data Cell = Indent
          | Cell (Integer, Integer) Char deriving (Eq, Ord)
instance Show Cell where
  show (Cell (i, j) c) = show (i, j) ++ " " ++ [c]
  show Indent          = "?"
type Cells = [Cell]

data Game = Game {
                   gameGrid  :: Grid Cell
                 , gameWords :: Map.Map String (Maybe Cells)
                 } deriving Show

makeGame :: Grid Char -> [String] -> Game
makeGame grid words =
  let cells = gridWithCoords grid
      initTuple word = (word, Nothing)
      initList = map initTuple words
      dict = Map.fromList initList
  in Game cells dict

makeRandomGrid :: (RandomGen g) => g -> Grid Char
makeRandomGrid gen =
  let (gen1, gen2) = split gen
      row = randomRs ('A', 'Z') gen1
  in row : makeRandomGrid gen2

fill :: Char -> Char -> Char
fill k c = if (c == '_') then k else c

fillInBlanks :: (RandomGen g) => g -> Grid Char -> Grid Char
fillInBlanks gen grid =
  let random = makeRandomGrid gen
  in zipGridWith fill random grid

totalWords :: Game -> Int
totalWords game = length . Map.keys $ gameWords game

score :: Game -> Int
score game = length . catMaybes . Map.elems $ gameWords game

playGame :: Game -> String -> Game
playGame game word | not $ Map.member word (gameWords game) = game
playGame game word =
  let grid = gameGrid game
      found = findWord word grid
  in case found of
       Nothing -> game
       Just cs ->
         let dict = gameWords game
             newDict = Map.insert word found dict
         in game { gameWords = newDict }

height :: Grid a -> Int
height = length

width :: Grid a -> Int
width = length . head

formatGrid :: Grid Cell -> String
formatGrid = unlines . mapOverGrid cell2char

outputGrid :: Grid Cell -> IO ()
outputGrid = putStrLn . formatGrid

formatGame :: Game -> String
formatGame game =
  formatGameGrid game
  ++ "\n"
  ++ "Score: "
  ++ (show $ score game)
  ++ "/"
  ++ (show $ totalWords game)

formatGameGrid :: Game -> String
formatGameGrid game =
  "\n" ++
  let grid = gameGrid game
      dict = gameWords game :: Map.Map String (Maybe Cells)
      cells = concat . catMaybes . Map.elems $ dict
      formatCell cell =
        let char = cell2char cell
        in if cell `elem` cells then char else toLower char
      charGrid = mapOverGrid formatCell grid
  in unlines charGrid

outputGame :: Game -> IO ()
outputGame = putStrLn . formatGame

mapOverGrid :: (Cell -> a) -> Grid Cell -> Grid a
mapOverGrid f = map . map $ f

cell2char :: Cell -> Char
cell2char (Cell _ a) = a
cell2char Indent     = '_'

getLines :: Grid Cell -> [Cells]
getLines grid =
  let horizontal = grid
      vertical   = transpose grid
      diagonal1  = diagonalize grid
      diagonal2  = diagonalize $ map reverse grid
      lines      = horizontal ++ vertical ++ diagonal1 ++ diagonal2
  in lines ++ map reverse lines

diagonalize :: Grid Cell -> Grid Cell
diagonalize = transpose . skew

skew :: Grid Cell -> Grid Cell
skew []     = []
skew (l:ls) = l : (skew $ map indent ls)
  where indent l = Indent : l

findWordInLine :: String -> Cells -> Maybe Cells
findWordInLine _ [] = Nothing
findWordInLine word line =
  let found = findWordInCellLinePrefix [] word line
  in case found of
    Nothing      -> findWordInLine word (tail line)
    res@(Just _) -> res

findWordInCellLinePrefix :: [Cell] -> String -> Cells -> Maybe Cells
findWordInCellLinePrefix acc (k:ks) (c:cs) | k == cell2char c
  = findWordInCellLinePrefix (c:acc) ks cs
findWordInCellLinePrefix acc [] _ = Just $ reverse acc
findWordInCellLinePrefix _ _ _    = Nothing

cells2string :: [Cell] -> String
cells2string = map cell2char

cells2strings :: Grid Cell -> Grid Char
cells2strings (l:ls) = (cells2string l) : cells2strings ls
cells2strings []     = []

-- searches grid for word written all directions
findWord :: String -> Grid Cell -> Maybe Cells
findWord word grid =
  let foundWords = map (findWordInLine word) (getLines grid)
  in listToMaybe $ catMaybes foundWords

-- currently generates 'lines' for each word in given list...
findWords :: [String] -> Grid Cell -> [Cells]
findWords ws grid =
  let foundWord w = findWord w grid
      foundWords  = filter (/= Nothing) (map foundWord ws)
  in catMaybes foundWords

gridWithCoords :: Grid Char -> Grid Cell
gridWithCoords = zipGrid coordsInf

cols :: Grid a -> [[Int]]
cols grid =
  let h = height grid
  in repeatN h $ take h [0..]

rows :: Grid a -> [[Int]]
rows grid =
  let w = width grid
  in map (repeatN w) (take w [0..])

rowsInf = map repeat [0..]
colsInf = repeat [0..]

coordsInf :: [[(Integer, Integer)]]
coordsInf = zipWith zip rowsInf colsInf

og :: Show a => [a] -> IO ()
og = putStrLn . unlines . map show

repeatN :: Int -> a -> [a]
repeatN n a = if (n < 2) then [a]
              else [a] ++ repeatN (n - 1) a

zipCell :: [(Integer, Integer)] -> String -> Cells
zipCell = zipWith Cell

zipGrid :: [[(Integer, Integer)]] -> Grid Char -> Grid Cell
zipGrid = zipWith zipCell

zipGridWith :: (a -> b -> c) -> Grid a -> Grid b -> Grid c
zipGridWith = zipWith . zipWith
