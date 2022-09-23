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


boardToString :: [Cell] -> [Char]
boardToString (a:b:c:d:e:f:g:h:i:j:rest) = "| " ++ show a ++ " | " ++ show b ++ " | " ++ show c ++ " | " ++ show d ++ " | " ++ show e ++ " | " ++ show f ++ " | " ++ show g ++ " | " ++ show h ++ " | " ++ show i ++ " | " ++ show j ++ " |"

lastRowToString :: Int -> String
lastRowToString x = "  " ++ show x ++ " "

-- IMPLEMENT
-- This is very initial state of your program
emptyState :: State
emptyState = State {rowData = [2,0,2,2,2,0,6,0,3,3], colData = [1,1,2,3,1,4,2,4,2,0], board = take 100 (repeat Blank), document = DNull}
-------------------- -------------------------------
-- IMPLEMENT
-- This adds game data to initial state 
gameStart :: State -> Document -> State
gameStart state d = State { rowData = rowData state, colData = colData state, board = board state, document = d }

-- IMPLEMENT
-- renders your game board
render :: State -> String
render state = do
    concat (rowNumBoard ++ (map lastRowToString (colData state)))
    where
        plainBoard  = map boardToString [firstLn, secondLn, thirdLn, fourthLn, fifthLn, sixthLn, seventhLn, eighthLn, ninthLn, tenthLn]
        rowNumBoard = [(plainBoard !! x) ++ " " ++ show (rowData state !! x) ++ "\n------------------------------------------\n" | x <- [0..9]]
        firstLn     = board state
        secondLn    = drop 10 (board state)
        thirdLn     = drop 20 (board state) 
        fourthLn    = drop 30 (board state)
        fifthLn     = drop 40 (board state) 
        sixthLn     = drop 50 (board state) 
        seventhLn   = drop 60 (board state)
        eighthLn    = drop 70 (board state)
        ninthLn     = drop 80 (board state) 
        tenthLn     = drop 90 (board state) 

-- IMPLEMENT
-- Make check from current state
mkCheck :: State -> Check
mkCheck _ = Check []

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
toggle state pos = do
    state { rowData = rowData state, colData = colData state, board = newBoard, document = document state} 
    where
        newCellType = if (board state) !! ((read (pos !! 0)) - 1) == Blank then Ship else Blank
        newBoard = toggleCell (board state) newCellType ((read (pos !! 0)))

-- IMPLEMENT
-- Adds hint data to the game state
hint :: State -> Document -> State
hint state h = state