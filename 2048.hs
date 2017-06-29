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


printState :: [[Int]] -> IO () -- print board
printState board = do
	putStr "\ESC[2J\ESC[2J\ESC[2J" --apparently this clears screen'
	putStrLn $ concat[(show line) ++ "\n"| line <- board]

addSpot 

gameLoop :: [[Int]] -> IO ()
gameLoop board = do
	putStr "todo"
	

main :: IO ()
main = do
		putStrLn "Welcome to 2048"
		putStrLn "use wasd to move"
		hSetBuffering stdin NoBuffering
		let board = [[0,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0]]
		