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
getEmptySpots board =
	let indices = elemIndices 0 $ concat board
	in [(floor((fromIntegral x)/4),mod x 4) | x <- indices]

gameLoop :: [[Int]] -> IO ()
gameLoop board = do
	printState board
	

main :: IO ()
main = do
		putStrLn "Welcome to 2048"
		putStrLn "use wasd to move"
		hSetBuffering stdin NoBuffering
		let board = [[0,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0]]
		gameLoop board
		