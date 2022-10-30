{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Lib2(renderDocument, hint, gameStart) where

import Types ( ToDocument(..), Document (DMap, DList, DInteger, DNull, DString), Check (coords), Coord (col, row) )
import Lib1 (State(..), getCoord, toggleShipHint, extractHintNumber, getDMapFromGameStart, getByKeyFromGameStart, traverseDMap, checkKey)

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
renderDocument d = 
    case d of 
        (DList x) -> "---\n" ++ unlines (toList  0 (DList x)) 
        (DMap  x) -> "---\n" ++ unlines (mapping 0 (DMap  x)) 
        (DInteger int) -> show int 
        (DString str)  -> str       
        (DNull)        -> "null"
    where 
        toList nestC doc = 
            case doc of 
            (DList []) -> []
            (DList ((DList x) : xs)) -> (h nestC "" "- " : toList (nestC + 2) (DList x)) ++ toList nestC (DList xs) 
            (DList ((DMap  x) : xs)) -> (h nestC "" "- " : mapping (nestC + 2) (DMap x)) ++ toList nestC (DList xs)
            (DList (x : xs))         -> (h nestC  "" "- " ++ convertPrimitiveToYaml x   )  : (toList nestC (DList xs)) 
        mapping nestC doc =
            case doc of
            (DMap []) -> []
            (DMap ((k, DMap v) : xs))   -> h nestC k ": " : (mapping (nestC + 2) (DMap v) ++ mapping nestC (DMap xs))
            (DMap ((k,  DList v) : xs)) -> h nestC k ": " : (toList (nestC + 2) (DList v) ++ mapping nestC (DMap xs))
            (DMap ((k, v) : xs))        -> (h nestC k ": " ++ convertPrimitiveToYaml v)     : (mapping nestC (DMap xs))
        h nestC k s = (wS nestC) ++ k ++ s 

-- whiteSpace
wS :: Int -> String
wS nestLevel = take nestLevel $ cycle " "

convertPrimitiveToYaml :: Document -> String
convertPrimitiveToYaml d = 
    case d of 
        (DInteger int) -> show int 
        (DString str)  -> str       
        (DNull)        -> "null"

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
        hint_number = hints
    }

parseGameStartDocument :: Document -> Either String (Int,[Int],[Int])
parseGameStartDocument d = do
    dmap <- getDMapFromGameStart d
    hintsTuple <- getByKeyFromGameStart dmap "number_of_hints"
    hints <- extractHintNumber hintsTuple 
    colsList <- getByKeyFromGameStart dmap "occupied_cols"
    rowsList <- getByKeyFromGameStart dmap "occupied_rows"
    colData <- traverseDMap colsList []
    rowData <- traverseDMap rowsList []
    isNumberOfColsGood <- validateDataQuantity colData "occupied_cols"
    isNumberOfRowsGood <- validateDataQuantity rowData "occupied_rows"
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
    coords <- parseHintDocument d (hint_number state)
    Right State {
        rowData = rowData state,
        colData = colData state,
        document = document state,
        board = toggleShipHint (board state) coords,
        hint_number = hint_number state
    }

parseHintDocument :: Document -> Int -> Either String [(Int, Int)]
parseHintDocument (DMap [(key, DList l)]) hints = do
    isKeyGood <- checkKey key "coords"
    coords <- getCoord l []
    if length coords > hints then Left "Wrong quantity of hints returned" else Right coords
parseHintDocument _ _ = Left "DMap [\"coords\", DList l] expected"