{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Lib2(renderDocument, hint, gameStart, parseGameStartDocument, parseHintDocument, checkKey, getCoord, toggleShipHint) where
import Types ( ToDocument(..), Document (DMap, DList, DInteger, DNull, DString), Check (coords), Coord (col, row) )
import Lib1 (append, toggleShipHint, Cell (Blank), State (..))

coordsToDMap :: Coord -> Document
coordsToDMap coord = DMap [("col", DInteger (col coord)), ("row", DInteger (row coord))]

-- IMPLEMENT
-- First, make Check an instance of ToDocument class
instance ToDocument Check
    where
    toDocument l = DMap [("coords", DList (map coordsToDMap (coords l)))]

emptyState :: State
emptyState = State {rowData = [], colData = [], board = take 100 (repeat Blank), document = DNull, hint_number = 0}

-- IMPLEMENT
-- Renders document to yaml
renderDocument :: Document -> String 
renderDocument (DMap [])   = "{}\n"
renderDocument (DList [])  = "[]\n"
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
h nestC k s = wS nestC ++ findK k ++ s 

findK :: String -> String  
findK k   
    | k == "" = "''"  
    | otherwise = k  

-- whiteSpace
wS :: Int -> String
wS nestLevel = take nestLevel $ cycle " "

-- render document su minusais
convertPrimitiveToYaml :: Document -> String
convertPrimitiveToYaml = f where
    f (DInteger i) =  if i >= 0 then show i else  ( "(" ++ show i ++ ")" )
    f (DString "") = "''"
    f (DString s) = show s
    f DNull = "null"

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
    _ <- checkKey key "head"
    number <- checkHeadInteger d
    return number

checkTail :: (String, Document) -> Either String Document
checkTail (key, d) = do
    _ <- checkKey key "tail"
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

checkKey :: String -> String -> Either String String
checkKey key expected = if key /= expected then Left "wrong key" else Right "good key"

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