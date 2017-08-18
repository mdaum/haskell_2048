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
import System.IO.Unsafe


printState :: [[Int]] -> IO () -- print board
printState board = do
	clearScreen -- from System.Console.ANSI
	putStrLn $ concat[(show line) ++ "\n"| line <- board]

getEmptySpots :: [[Int]] -> [(Int,Int)]
getEmptySpots board = [(floor((fromIntegral x)/4),mod x 4) | x <- (elemIndices 0 $ concat board)]

addSpot :: [[Int]] -> [[Int]]
addSpot board = rewriteBoard board spot toAdd
	where { empties = getEmptySpots board ;
	spotNum = getRand (0, length empties-1) ;
	spot = (empties !! spotNum) ;
	addNum = getRand(0,9) ;
	toAdd = ([2,2,2,2,2,2,2,2,2,4] !! addNum) }
	
getRand :: (Int,Int) -> Int
getRand range = unsafePerformIO (getStdRandom (randomR range))

--will rewrite the board with added spot
rewriteBoard :: [[Int]] -> (Int,Int) -> Int -> [[Int]]
rewriteBoard board (row,col) toAdd = b4 ++ [target] ++ after
	where { b4 = take row board ; --take everything b4 target row
		  target = take col (board!!row) ++ [toAdd] ++ drop (col +1) (board!!row) ;
		  after = drop (row+1) board }--everything after target row
			  

			  
gameLoop :: [[Int]] -> IO ()
gameLoop board = do
	let newBoard = addSpot board
	printState newBoard
	action <- getAction ['w', 'a', 's', 'd']
	let newerBoard = handleAction action newBoard
	let over = checkGameOver newerBoard
	if (over) then putStrLn "game over" else gameLoop newerBoard
	
getAction :: [Char] -> IO Char
getAction chars = do
	c <- hGetChar stdin
	if (c `elem` chars) then return c else getAction chars

handleAction :: Char -> [[Int]] -> [[Int]]
handleAction 'w' board = moveUp board
handleAction 'a' board = moveLeft board
handleAction 's' board = moveDown board
handleAction 'd' board = moveRight board


moveUp :: [[Int]] -> [[Int]]
moveUp board = transpose $ moveLeft $ transpose board 

moveDown :: [[Int]] -> [[Int]]
moveDown board = transpose $ moveRight $ transpose board

moveRight :: [[Int]] -> [[Int]]
moveRight board = map (reverse.move.reverse) board

moveLeft :: [[Int]] -> [[Int]]
moveLeft board = map move board

move :: [Int] -> [Int] -- will move a row left...to be used by rest of directions
move [] = []
move (x:[]) = [x]
move (0:xs) = move xs ++ [0]
move (x:0:xs) = (move (x:xs)) ++ [0]
move(x:y:xs) | x == y = ((x + y) : (move xs)) ++ [0]
			 | otherwise = (x:(move (y:xs)))
	

isFull :: [[Int]] -> Bool
isFull board = notElem 0 (concat board)

checkGameOver:: [[Int]] -> Bool
checkGameOver board = (isFull $ moveLeft board) && (isFull $ moveRight board) && (isFull $ moveUp board) && (isFull $ moveDown board)

main :: IO ()
main = do
		putStrLn "Welcome to 2048"
		putStrLn "use wasd to move"
		hSetBuffering stdin NoBuffering
		let board = [[0,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0]]
		gameLoop board
		