-- A program to process text mode Metro screenshots
-- Challenge from: https://www.reddit.com/r/dailyprogrammer/comments/2uo3yf/20150204_challenge_200_intermediate_metro_tile/

import Data.Maybe						-- To make Maybe handling easier
import Data.List						-- So we can sort
import System.IO						-- For file loading

------------------ Some types we'll use ------------------

data Square = Square {
					startColumn		:: Int,
					startRow		:: Int,
					squareWidth 	:: Int,
					squareHeight 	:: Int,
					theChar			:: Char
				}
				
type LiveSquares = [Square]
type DeadSquares = [Square]
				
------------------ Instances so we can show things easily, sort, and check equality ------------------

instance Show Square where
	show (Square c r w h ch)		= show w ++ "x" ++ show h ++ " tile of character '" ++
										[ch] ++ "' located at (" ++ show c ++ "," ++ show r ++ ")"

instance Eq Square where
	(==) (Square col _ _ _ ch) (Square col2 _ _ _ ch2)
									= (col == col2) && (ch == ch2)

instance Ord Square where
	x `compare` y = if byRow == EQ then byChar else byRow
		where
			byRow = (startColumn x) `compare` (startColumn y)
			byChar = (theChar x) `compare` (theChar y)
									
------------------ Functions to do the work ------------------

lineToSize :: String -> (Int, Int)
lineToSize s = (read w, read h)
		where
			(w:h:_) = words s

lineToSquares :: Int -> String -> (Int, [Square])
lineToSquares r s = (r + 1, filter (\x -> theChar x /= '.') squares)
		where
			parts			= group s
			(_, squares)	= mapAccumL buildSquare 0 parts
			buildSquare a l = (a + (length l), Square a r (length l) 1 (head l))

combineLines :: (LiveSquares, DeadSquares) -> [Square] -> (LiveSquares, DeadSquares)
combineLines (oldSquares, oldLive) newLine = (updatedSquares ++ newSquares, oldLive ++ gone)
		where
			(here, gone)	= partition (\x -> x `elem` newLine) oldSquares
			updatedSquares	= map (\x -> x{squareHeight = (squareHeight x) + 1}) here
			newSquares	= filter (not . (\x -> x `elem` oldSquares)) newLine

------------------ Our main function, to do the work ------------------

main = do
	fileText <- readFile "test4.txt"
	
	let fileLines			= lines fileText
	
	let (width, height)		= lineToSize (head fileLines)
	let rows				= take height (tail fileLines)
	let (_, squareRows)		= mapAccumL lineToSquares 0 rows
	
	let (lastLive, lastDead)	= foldl combineLines (head squareRows, []) (tail squareRows)
	
	let sortedRows = sort (lastLive ++ lastDead)
	
	mapM (putStrLn . show) sortedRows
