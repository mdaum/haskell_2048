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

addSpot :: [[Int]] -> StdGen -> ([[Int]], StdGen)
addSpot board g = do
	let { empties = getEmptySpots board ;
	(spotNum, g1) = randomR (0, length empties-1) g;
	spot = (empties !! spotNum);
	(addNum,g2) = randomR (0,9) g1;
	toAdd = ([2,2,2,2,2,2,2,2,2,4] !! addNum);
	newBoard = rewriteBoard board spot addNum}
	return (newBoard, g2)
--will rewrite the board with added spot
rewriteBoard :: [[Int]] -> (Int,Int) -> Int -> [[Int]]
rewriteBoard board (row,col) toAdd = b4 ++ [target] ++ after
	where { b4 = take row board ; --take everything b4 target row
		  target = take col (board!!row) ++ [toAdd] ++ drop (col +1) (board!!row) ;
		  after = drop (row+1) board }--everything after target row}
			  
gameLoop :: [[Int]] -> StdGen -> IO ()
gameLoop board g = do
	(newBoard,newG) <- addSpot board g
	printState newBoard
	continue <- getChar
	gameLoop newBoard newG

isFull :: [[Int]] -> Bool
isFull board = notElem 0 (concat board)

main :: IO ()
main = do
		putStrLn "Welcome to 2048"
		putStrLn "use wasd to move"
		hSetBuffering stdin NoBuffering
		let board = [[0,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0]]
		g <- newStdGen
		gameLoop board g
		