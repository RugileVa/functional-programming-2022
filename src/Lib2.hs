{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Lib2(renderDocument, hint, gameStart) where

import Types ( ToDocument(..), Document (DMap, DList, DInteger), Check (coords), Coord (col, row) )
import Lib1 (State(..), getCoord, toggleShipHint, traverseDMap, takeRowsList, takeColsList)

coordsToDMap :: Coord -> Document
coordsToDMap coord = DMap [("col", DInteger (col coord)), ("row", DInteger (row coord))]

-- IMPLEMENT
-- First, make Check an instance of ToDocument class
instance ToDocument Check 
    where
    toDocument l = DList (map coordsToDMap (coords l))

-- IMPLEMENT
-- Renders document to yaml
renderDocument :: Document -> String
renderDocument _ = error "Implement me"

-- IMPLEMENT
-- This adds game data to initial state
-- Errors are reported via Either but not error 
gameStart :: State -> Document -> Either String State
gameStart state d = Right State { 
    rowData = traverseDMap (takeRowsList d) [], 
    colData = traverseDMap (takeColsList d) [],
    board = board state, 
    document = d 
}

-- IMPLEMENT
-- Adds hint data to the game state
-- Errors are reported via Either but not error 
hint :: State -> Document -> Either String State
hint state (DMap [(_, DList l)]) = Right State {
    rowData = rowData state, 
    colData = colData state, 
    document = document state, 
    board = toggleShipHint (board state) (getCoord l [])
}
-- hint state (DMap (([], (DList [])):_:_)) = Right state
-- hint state (DMap (([], (DList (_:_))):_:_)) = Right state
-- hint state (DMap (((_:_), (DList (_:_))):_:_)) = Right state
-- hint state (DMap (([_], (DList [])):_:_)) = Right state
-- hint state (DMap (((_:_:_), (DList [])):_:_)) = Right state
-- hint state (DMap [([], (DMap _))]) = Right state
-- hint state (DMap (([], (DMap [])):_:_)) = Right state
-- hint state (DMap (([], (DMap (_:_))):_:_)) = Right state
-- hint state (DMap [([], DInteger _)]) = Right state
