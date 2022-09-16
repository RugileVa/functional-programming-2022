{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE DeriveGeneric #-}
module Lib1(
    State, emptyState, gameStart, render, mkCheck, toggle, hint
) where

import Types
import Data.Aeson
import Data.ByteString.Lazy.Char8 (unpack)
import GHC.Generics (Generic)
import Data.ByteString.Lazy.Char8 (pack, putStrLn)
import Prelude

-- This is a state of your game.
-- It must contain all values you might need during a game:
-- number of occupied rows/cols, hints, occupied cells,..
-- You can change the right hand side as you wish but please
-- keep the type name as is
data State = State [String] 
    deriving Show

data Square = Square {
    coord :: Coord,
    chr :: [Char]
} deriving (Generic, Show, Eq)
instance ToJSON Square
instance FromJSON Square
instance FromJSON Coord

-- IMPLEMENT
-- This is very initial state of your program
emptyState :: State
emptyState = State [unpack (encode [
    Square {coord = Coord {col = 1, row = 1}, chr = " "}, 
    Square {coord = Coord {col = 2, row = 1}, chr = " "}, 
    Square {coord = Coord {col = 3, row = 1}, chr = " "},
    Square {coord = Coord {col = 1, row = 2}, chr = " "}, 
    Square {coord = Coord {col = 2, row = 2}, chr = " "}, 
    Square {coord = Coord {col = 3, row = 2}, chr = " "},
    Square {coord = Coord {col = 1, row = 3}, chr = " "}, 
    Square {coord = Coord {col = 2, row = 3}, chr = " "}, 
    Square {coord = Coord {col = 3, row = 3}, chr = " "}])]
-------------------- -------------------------------
-- IMPLEMENT
-- This adds game data to initial state 
gameStart :: State -> Document -> State
gameStart (State l) d = State $ ("Game started: " ++ show d) : l

-- gets the last element of a list
listLast :: [a] -> a
listLast [x] = x --base case is when there's just one element remaining
listLast (_:xs) = listLast xs --if there's anything in the head, continue until there's one element left
listLast [] = error "Can't do last of an empty list!"

showBoardLine :: [Square] -> [Char]
showBoardLine (a:b:c:rest) = "| " ++ chr a ++ " | " ++ chr b ++ " | " ++ chr c ++ " |\n"

boardToString :: [[Square]] -> String
boardToString lst = concatMap showBoardLine lst

-- IMPLEMENT
-- renders your game board
render :: State -> String
render (State l) = do
    case decode (pack (listLast l)) :: Maybe [Square] of
        Nothing -> show "Error: can't decode the file."
        Just board -> do
            show $ boardToString [row1, row2, row3]
                where
                    row1 = board
                    row2 = drop 3 board
                    row3 = drop 6 board

-- IMPLEMENT
-- Make check from current state
mkCheck :: State -> Check
mkCheck _ = Check []

-- IMPLEMENT
-- Toggle state's value
-- Receive raw user input tokens
toggle :: State -> [String] -> State
toggle (State l) t = State $ ("Toggle " ++ show t) : l

-- IMPLEMENT
-- Adds hint data to the game state
hint :: State -> Document -> State
hint (State l) h = State $ ("Hint " ++ show h) : l