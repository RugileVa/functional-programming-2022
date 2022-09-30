{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
module Lib1(
    State, emptyState, gameStart, render, mkCheck, toggle, hint
) where

import Types
import Prelude
import Types (Document(DMap))

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
    board :: [Cell]
} deriving Show

-- IMPLEMENT
-- This is very initial state of your program
emptyState :: State
emptyState = State {rowData = [], colData = [], board = take 100 (repeat Blank), document = DNull}
-------------------- -------------------------------

takeColsList :: Document -> Document
takeColsList (DMap l) = dmap
    where
        (str, dmap) = head (drop 1 l)

takeRowsList :: Document -> Document
takeRowsList (DMap l) = dmap
    where 
        (str, dmap) = head (drop 2 l)

append :: Int -> [Int] -> [Int]
append a [] = [a]
append a (x:xs) = x : append a xs

traverseDMap :: Document -> [Int] -> [Int]
traverseDMap (DMap [(_, DInteger num),(_, DNull)]) numbers  = append num numbers
traverseDMap (DMap [(_, DInteger num),(_, dmap)]) numbers = traverseDMap dmap (append num numbers)

-- IMPLEMENT
-- This adds game data to initial state 
gameStart :: State -> Document -> State
gameStart state d = State { 
    rowData = traverseDMap (takeRowsList d) [], 
    colData = traverseDMap (takeColsList d) [],
    board = board state, 
    document = d 
}

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
        newCellType = if board state !! (((read (head pos) - 1) * 10 + read (pos !! 1)) - 1) == Blank then Ship else Blank
        newBoard = toggleCell (board state) newCellType ((read (head pos) - 1) * 10 + read (pos !! 1))

-- IMPLEMENT
-- Adds hint data to the game state
hint :: State -> Document -> State
hint state h = state