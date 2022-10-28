{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Lib2(renderDocument, hint, gameStart) where

import Types ( ToDocument(..), Document (DMap, DList, DInteger, DNull, DString), Check (coords), Coord (col, row) )
import Lib1 (State(..), getCoord, toggleShipHint, extractHintNumber, getDMapFromGameStart, getByKeyFromGameStart, traverseDMap, checkKey)
import Data.Yaml (YamlMark(yamlColumn))
import Data.Aeson (JSONKeyOptions(keyModifier))

coordsToDMap :: Coord -> Document
coordsToDMap coord = DMap [("col", DInteger (col coord)), ("row", DInteger (row coord))]

-- IMPLEMENT
-- First, make Check an instance of ToDocument class
instance ToDocument Check
    where
    toDocument l = DMap [("coords", DList (map coordsToDMap (coords l)))]

-- IMPLEMENT
-- Renders document to yaml
renderDocument :: Document -> String
renderDocument DNull = "null"
renderDocument (DInteger int) = show int
renderDocument (DList l) = "---\n" ++ concatMap convertListToYaml l
renderDocument (DMap [(str, DList l)]) = str ++ ":" ++ concatMap mapCoordToYaml l

convertListToYaml :: Document -> String
convertListToYaml (DInteger int)= "- " ++ show int ++ "\n"
convertListToYaml (DString str)= "- " ++ str ++ "\n"

mapCoordToYaml :: Document -> String
mapCoordToYaml (DMap [(str1, DInteger int1), (str2, DInteger int2)]) =
    "\n- " ++ str1 ++ ": " ++ show int1 ++ "\n  " ++ str2 ++ ": " ++ show int2

-- IMPLEMENT
-- This adds game data to initial state
-- Errors are reported via Either but not error 
gameStart :: State -> Document -> Either String State
gameStart state d = do
    (hints, newRowData, newColData) <- parseGameStartDocument d
    Right State {
        rowData = newRowData,
        colData = newColData,
        board = board state,
        document = d,
        hints = hints
    }

parseGameStartDocument :: Document -> Either String (Int,[Int],[Int])
parseGameStartDocument d = do
    dmap <- getDMapFromGameStart d
    hintsTuple <- getByKeyFromGameStart dmap "number_of_hints"
    hints <- extractHintNumber hintsTuple 
    rowsList <- getByKeyFromGameStart dmap "occupied_rows"
    colsList <- getByKeyFromGameStart dmap "occupied_cols"
    rowData <- traverseDMap (rowsList) []
    colData <- traverseDMap (colsList) []
    isNumberOfRowsGood <- validateDataQuantity rowData "occupied_rows"
    isNumberOfColsGood <- validateDataQuantity rowData "occupied_cols"
    return (hints, rowData, colData)

validateDataQuantity :: [Int] -> String -> Either String String
validateDataQuantity l str = 
    if length l /= 10 
        then Left ("there must be 10 values in the " ++ str ++ " element")
        else Right "number is good"

-- IMPLEMENT
-- Adds hint data to the game state
-- Errors are reported via Either but not error 
hint :: State -> Document -> Either String State
hint state d = do 
    coords <- parseHintDocument d (hints state)
    Right State {
        rowData = rowData state,
        colData = colData state,
        document = document state,
        board = toggleShipHint (board state) coords,
        hints = hints state
    }

parseHintDocument :: Document -> Int -> Either String [(Int, Int)]
parseHintDocument (DMap [(key, DList l)]) hints = do
    isKeyGood <- checkKey key "coords"
    coords <- getCoord l []
    if length coords > hints then Left "Wrong quantity of hints returned" else Right coords
parseHintDocument _ _ = Left "DMap [\"coords\", DList l] expected"


-- hint state (DMap (([], (DList [])):_:_)) = Right state
-- hint state (DMap (([], (DList (_:_))):_:_)) = Right state
-- hint state (DMap (((_:_), (DList (_:_))):_:_)) = Right state
-- hint state (DMap (([_], (DList [])):_:_)) = Right state
-- hint state (DMap (((_:_:_), (DList [])):_:_)) = Right state
-- hint state (DMap [([], (DMap _))]) = Right state
-- hint state (DMap (([], (DMap [])):_:_)) = Right state
-- hint state (DMap (([], (DMap (_:_))):_:_)) = Right state
-- hint state (DMap [([], DInteger _)]) = Right state
