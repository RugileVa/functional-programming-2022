{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
module Lib1(
    State(..), Cell(..), append, emptyState, gameStart, hint, render, mkCheck, toggle, toggleShipHint, toggleCell, removeNth, fromDocument, toDocument
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
    hint_number :: Int
} deriving (Eq, Show)


instance ToDocument State where 
    toDocument state = DList[DMap[("cols", DList (reverse $ tolist (rowData state) []))], DMap[("rows", DList (reverse $ tolist (colData state) []))], DMap[("board", DList (reverse $ mapcells (board state) []))]]

tolist :: [Int] -> [Document] -> [Document] 
tolist [] list = list 
tolist xs list = Prelude.foldl (\list x -> DInteger x : list) list xs

mapcells :: [Cell] -> [Document] -> [Document]
mapcells [] list = list
mapcells xs list = Prelude.foldl (\list x -> DString (show x) : list) list xs 

instance FromDocument State where 
    fromDocument doc = do
        (newRowData, newColData, newBoard) <- findDocument doc
        Right State {
        rowData = newRowData,
        colData = newColData,
        board = newBoard,
        document = DNull,
        hint_number = 0
        }

findDocument :: Document -> Either String ([Int],[Int],[Cell])
findDocument (DList d) = do
    case getByKey d  "cols" of 
        Left _ -> Left "Could not parse document"
        Right (DList doc1) -> do 
            let coldata = traverselist doc1 []
            case getByKey d "rows" of 
                Left _ -> Left "Could not parse document"
                Right (DList doc2) -> do 
                    let rowdata = traverselist doc2 []
                    case getByKey d "board" of 
                        Left _ -> Left "Could not parse document"
                        Right (DList doc3) -> do 
                            let boarddata = traverseboard doc3 []
                            return (coldata, rowdata, boarddata)

getByKey :: [Document] -> String -> Either String Document
getByKey [] key = Left $ "Error when parsing a document in key"
getByKey (DMap[(str, d)]:xs) key = if key == str then Right d else getByKey xs key

traverselist :: [Document] -> [Int] -> [Int]
traverselist [] list = list 
traverselist (DInteger x:xs) list = traverselist xs (x:list)

traverseboard :: [Document] -> [Cell] -> [Cell]
traverseboard [] list = list 
traverseboard (DString " ":xs) list = traverseboard xs (Blank:list)
traverseboard (DString "x":xs) list = traverseboard xs (Ship:list)

-- IMPLEMENT
-- This is very initial state of your program
emptyState :: State
emptyState = State {rowData = [], colData = [], board = take 100 (repeat Blank), document = DNull}

gameStart :: State -> Document -> State
gameStart state d = State { 
    rowData = traverseDMap (takeRowsList d) [], 
    colData = traverseDMap (takeColsList d) [],
    board = board state, 
    document = d 
}

traverseDMap :: Document -> [Int] -> [Int]
traverseDMap (DMap [(_, DInteger num),(_, DNull)]) numbers  = append num numbers
traverseDMap (DMap [(_, DInteger num),(_, dmap)]) numbers = traverseDMap dmap (append num numbers)

takeColsList :: Document -> Document
takeColsList (DMap l) = dmap
    where
        (_, dmap) = head (drop 1 l)
takeRowsList :: Document -> Document
takeRowsList (DMap l) = dmap
    where 
        (_, dmap) = head (drop 2 l)

append :: Int -> [Int] -> [Int]
append a [] = [a]
append a (x:xs) = x : append a xs

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

hint :: State -> Document -> State
hint state (DMap [(_, DList l)]) = State {
    rowData = rowData state, 
    colData = colData state, 
    document = document state, 
    board = toggleShipHint (board state) (getCoord l [])
}

getCoord :: [Document] -> [(Int, Int)] -> [(Int, Int)]
getCoord [] coords = coords
getCoord (dmap:xs) coords = getCoord xs (extractNumbers dmap : coords)

extractNumbers :: Document -> (Int, Int)
extractNumbers (DMap [(_,DInteger x),(_,DInteger y)]) = (x, y)