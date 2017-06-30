{-
2048 in Haskell
Maxwel Daum
Use wasd to move
-}

import Prelude
import Data.Char
import Data.List
import System.IO
import System.Random
import Text.Printf
import System.Console.ANSI


printState :: [[Int]] -> IO () -- print board
printState board = do
	clearScreen -- from System.Console.ANSI
	putStrLn $ concat[(show line) ++ "\n"| line <- board]

getEmptySpots :: [[Int]] -> [(Int,Int)]
getEmptySpots board = [(floor((fromIntegral x)/4),mod x 4) | x <- (elemIndices 0 $ concat board)]

addSpot :: [[Int]] -> [[Int]]
addSpot board = do
	let { empties = getEmptySpots board ;
	spot = (empties !! (randomRIO(0,length empties-1))) ;
	toAdd = ([2,2,2,2,2,2,2,2,2,4] !! (randomRIO(0,9))) }
	return rewriteBoard board spot toAdd

--will rewrite the board with added spot
rewriteBoard :: [[Int]] -> (Int,Int) -> Int -> [[Int]]
rewriteBoard board (row,col) toAdd = b4 ++ [target] ++ after
	where { b4 = take row board ; --take everything b4 target row
		  after = drop (row+1) ; --everything after target row
		  target = take col (grid!!row) ++ [toAdd] ++ drop (col +1) (grid!!row) }
			  
gameLoop :: [[Int]] -> IO ()
gameLoop board = do
	newBoard <- addSpot board
	printState newBoard
	continue <- getChar
	gameLoop newBoard

isFull :: [[Int]] -> Bool
isFull board = notElem 0 (concat board)

main :: IO ()
main = do
		putStrLn "Welcome to 2048"
		putStrLn "use wasd to move"
		hSetBuffering stdin NoBuffering
		let board = [[0,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0]]
		gameLoop board
		