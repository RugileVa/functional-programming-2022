{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Lib1(
    State(..), emptyState, extractHintNumber, render, mkCheck, toggle, getCoord, getDMapFromGameStart, getByKeyFromGameStart, extractNumbers, toggleShipHint, toggleCell, removeNth, traverseDMap, checkKey
) where

import Types
import Prelude

data Cell =  Blank 
            | Ship deriving Eq
instance Show Cell where
    show Blank  = " "
    show Ship   = "x"

-- This is a state of your game.
-- It must contain all values you might need during a game:
-- number of occupied rows/cols, hints, occupied cells,..
-- You can change the right hand side as you wish but please
-- keep the type name as is
data State = State {
    rowData :: [Int],
    colData :: [Int],
    document :: Document,
    board :: [Cell],
    hints :: Int
} deriving Show

-- IMPLEMENT
-- This is very initial state of your program
emptyState :: State
emptyState = State {rowData = [], colData = [], board = take 100 (repeat Blank), document = DNull, hints = 0}
-------------------- -------------------------------

append :: Int -> [Int] -> [Int]
append a [] = [a]
append a (x:xs) = x : append a xs

getByKeyFromGameStart :: [(String, Document)] -> String -> Either String Document
getByKeyFromGameStart [] key = Left ("Element with key \"" ++ key ++ "\" not found in gameStart document") 
getByKeyFromGameStart ((str, d):xs) key = if key == str then Right d else getByKeyFromGameStart xs key

getDMapFromGameStart :: Document -> Either String [(String, Document)]
getDMapFromGameStart (DMap l) = Right l
getDMapFromGameStart _ = Left "DMap expected at gameStart"

traverseDMap :: Document -> [Int] -> Either String [Int]
traverseDMap DNull numbers = Right numbers
traverseDMap d numbers = do
    headTailList <- extractHeadTail d
    num <- checkHead (head headTailList)
    document <- checkTail (headTailList !! 1)
    traverseDMap document (append num numbers)

extractHeadTail :: Document -> Either String [(String, Document)]
extractHeadTail (DMap l) = Right l
extractHeadTail _ = Left "row and col info in the gameStart document must be comprised of DMaps"

checkHead :: (String, Document) -> Either String Int
checkHead (key, d) = do
    isKeyGood <- checkKey key "head"
    number <- checkHeadInteger d
    return number

checkTail :: (String, Document) -> Either String Document
checkTail (key, d) = do
    isKeyGood <- checkKey key "tail"
    d <- checkTailDocument d
    return d

checkHeadInteger :: Document -> Either String Int
checkHeadInteger (DInteger int) = do
    if (int < 0) || (int > 10) 
        then Left "\"head\" element must be a DInteger with value between 0 and 10 (inclusive)"
        else Right int
checkHeadInteger _ = Left "\"head\" element must be an DInteger"

checkTailDocument :: Document -> Either String Document
checkTailDocument DNull = Right DNull
checkTailDocument (DMap headTail) = Right (DMap headTail)
checkTailDocument _ = Left "\"tail\" element must be an DMap or DNull"

extractHintNumber :: Document -> Either String Int
extractHintNumber (DInteger int) = Right int
extractHintNumber _ = Left "DInteger expected in \"number_of_hints\""

-- IMPLEMENT
-- renders your game board

intersperseRow :: [Cell] -> String
intersperseRow ys = "| " ++ concatMap (\y -> show y ++ " | ") ys

lastRowToString :: Int -> String
lastRowToString x = "  " ++ show x ++ " "


render :: State -> String
render state = do
    concat (rowNumBoard ++ map lastRowToString (colData state))
    where
        plainBoard  = map intersperseRow [firstLn, secondLn, thirdLn, fourthLn, fifthLn, sixthLn, seventhLn, eighthLn, ninthLn, tenthLn]
        rowNumBoard = [(plainBoard !! x) ++ " " ++ show (rowData state !! x) ++ "\n------------------------------------------\n" | x <- [0..9]]
        firstLn     = take 10 $ board state
        secondLn    = take 10 $ drop 10 (board state)
        thirdLn     = take 10 $ drop 20 (board state) 
        fourthLn    = take 10 $ drop 30 (board state)
        fifthLn     = take 10 $ drop 40 (board state) 
        sixthLn     = take 10 $ drop 50 (board state)
        seventhLn   = take 10 $ drop 60 (board state)
        eighthLn    = take 10 $ drop 70 (board state)
        ninthLn     = take 10 $ drop 80 (board state) 
        tenthLn     = take 10 $ drop 90 (board state) 

-- IMPLEMENT
-- Make check from current state
positions :: (a -> Bool) -> [a] -> [Int] 
positions p l = positionsIndex 0 p l
     where
     positionsIndex :: Int -> (a -> Bool) -> [a] -> [Int] 
     positionsIndex _ _ []     = []
     positionsIndex index p (x:xs) = if p x then index : positionsIndex (index + 1) p xs else positionsIndex (index + 1) p xs

predicate :: Cell -> Bool
predicate c = c == Ship 

findXfrom1Dto2D :: Int -> Int
findXfrom1Dto2D d = mod d 10 
     
findYfrom1Dto2D :: Int-> Int
findYfrom1Dto2D d = div d 10 

newListXY :: [Cell] -> [(Int,Int)]
newListXY l = zip li1 li2 
     where li' = positions predicate l
           li1 = map findXfrom1Dto2D li'
           li2 = map findYfrom1Dto2D li'

toCoord :: [(Int, Int)] -> [Coord] 
toCoord [] = []
toCoord ((x,y):sd) = Coord x y : toCoord sd

mkCheck :: State -> Check
mkCheck state = Check (toCoord (newListXY (board state)) )


-- Removes the Nth item (index being N-1) from a list
removeNth :: Int -> [a] -> ([a], [a])
removeNth index lst = (left, right)
    where 
        (left, ys) = splitAt (index - 1) lst
        right = drop 1 ys

-- Given a board, piece, and index to place it in, place piece
toggleCell :: [a] -> a -> Int -> [a]
toggleCell board piece n = xs ++ [piece] ++ ys
    where (xs, ys) = removeNth n board

-- IMPLEMENT
-- Toggle state's value
-- Receive raw user input tokens
toggle :: State -> [String] -> State
toggle state pos = state { rowData = rowData state, colData = colData state, board = newBoard, document = document state} 
    where 
        newCellType = if board state !! ((read (head pos) - 1) + (read (pos !! 1) - 1) * 10) == Blank then Ship else Blank
        newBoard = toggleCell (board state) newCellType (read (head pos) + (read (pos !! 1) - 1) * 10)

toggleShipHint :: [Cell] -> [(Int, Int)] -> [Cell]
toggleShipHint board [] = board
toggleShipHint board ((x, y):xs) = toggleShipHint newBoard xs
    where 
        newBoard = toggleCell board Ship (x + 1 + y * 10)


getCoord :: [Document] -> [(Int, Int)] -> Either String [(Int, Int)]
getCoord [] coords = Right coords
getCoord (dmap:xs) coords = do
    numbers <- extractNumbers dmap
    getCoord xs (numbers : coords)


extractNumbers :: Document -> Either String (Int, Int)
extractNumbers (DMap [(colKey,DInteger x),(rowKey,DInteger y)]) = do
    isColKeyGood <- checkKey colKey "col"
    isRowKeyGood <- checkKey rowKey "row"

    if (x > 9) || (x < 0) || (y > 9) || (y < 0) 
        then Left "integer is out of bounds"
        else Right (x, y) 
         
checkKey :: String -> String -> Either String String
checkKey key expected = if key /= expected then Left "wrong key" else Right "good key"