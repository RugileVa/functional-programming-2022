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
renderDocument (DMap [])   = "---\n[]"
renderDocument (DList [])  = "---\n[]"
renderDocument (DList x)   = "---\n" ++ unlines (toList  0 (DList x)) 
renderDocument (DMap  x)   = "---\n" ++ unlines (mapping 0 (DMap  x)) 
renderDocument  d          = convertPrimitiveToYaml d

toList :: Int -> Document -> [String]
toList nestC doc = 
            case doc of 
            (DList []) -> []
            (DList ((DList []) : xs))  -> ((wS nestC ++ "- []") : toList nestC (DList xs))
            (DList ((DList x) : xs))   -> ((wS nestC ++ "- "  ) : toList (nestC + 2) (DList x)) ++ toList nestC (DList xs) 
            (DList ((DMap []) : xs))   -> ((wS nestC ++ "- []") : toList nestC (DList xs))
            (DList ((DMap  x) : xs))   -> ((wS nestC ++ "- "  ) : mapping (nestC + 2) (DMap x)) ++ toList nestC (DList xs)
            (DList (x : xs))           -> ( wS nestC ++ "- " ++ convertPrimitiveToYaml x   )  : (toList nestC (DList xs)) 

mapping :: Int -> Document -> [String]
mapping nestC doc =
            case doc of
            (DMap []) -> []
            (DMap ((k, DMap []) : xs))   -> (h nestC k ": " ++ "[]")  :  mapping nestC (DMap xs)
            (DMap ((k, DMap v) : xs))    -> h nestC k ": " : (mapping (nestC + 2) (DMap v) ++ mapping nestC (DMap xs))
            (DMap ((k,  DList []) : xs)) -> (h nestC k ": " ++ "[]")  : mapping nestC (DMap xs)
            (DMap ((k,  DList v) : xs))  -> h nestC k ": " : (toList (nestC + 2) (DList v) ++ mapping nestC (DMap xs))
            (DMap ((k, v) : xs))         -> (h nestC k ": " ++ convertPrimitiveToYaml v)     : (mapping nestC (DMap xs))

h :: Int -> String -> String -> String 
h nestC k s = (wS nestC) ++ findK k ++ s 

findK :: String -> String  
findK k   
    | (k == "") = "''"  
    | otherwise = k  

-- whiteSpace
wS :: Int -> String
wS nestLevel = take nestLevel $ cycle " "

convertPrimitiveToYaml :: Document -> String
convertPrimitiveToYaml = f where
    f (DInteger i) = show i
    f (DString "") = "''"
    f (DString s) = s
    f DNull = "null"

-- Sąrašas mažų klaidų, bei kaip pataisėme testą, kad jis passint'ų: 
-- 1) Neveikė empty DMap (pataisėme)
-- 2) Neveikė empty DList (pataisėme)
-- 3) Neapdorojo tuščcio string'o DString konstruktoriuje ir DMap key. Savo funkcijoje tuščią string'ą vertėme į '', o ne \"\"" (pataisėme)
-- 4) Duotame ir mūsų testo rezultate skiriasi indentacija (pas mus daugiau tarpų nuo krašto), tačiau abi yra validžios, todėl palikome savąją.
-- 5) DMap [("", DNull)] teste laikomas null, mes palikome tai kaip key-value pair ('': null)
-- 6) nested mappings, kitaip negu duotame teste neiškyreme brūkšneliu, o indentacija. Pasitikrinome, kad tai validus yaml dokumentas. 
-- 6) DList elementai ir duotame ir mūsų teste yra pradedami "- ", tačiau jei tai nested DList ar DMap pradedame jį
--    iš naujos eilutės. Žr. 2.3 pvz iš YAML specification ir 2.5 pvz.


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