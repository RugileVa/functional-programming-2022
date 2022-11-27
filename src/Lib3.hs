{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Lib3 (parseDocument, hint, gameStart, GameStart, Hint) where
import Lib2 (parseGameStartDocument, checkKey, getCoord, toggleShipHint)
import Types ( Document(..), FromDocument, fromDocument )
import Lib1 (State(..), Cell(..))
import Control.Applicative
import Data.Char

-- IMPLEMENT
-- Parses a document from yaml
parseDocument :: String -> Either String Document 
parseDocument str = fst <$> runParser parseDoc str

newtype Parser a = Parser { runParser :: String -> Either String (a, String) }

instance Functor Parser where
    --fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (Parser p) = Parser $ \input ->  
        case p input of
        Left err -> Left err
        Right (output, rest) -> Right (f output, rest)

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure a = Parser $ \input -> Right (a, input)
     -- <*> :: Parser (a -> b) -> Parser a -> Parser b
    Parser f <*> Parser p = Parser $ \input ->
        case f input of
        Left err -> Left err
        Right (f', rest) ->
            case p rest of
            Left err -> Left err
            Right (output, rest') -> Right (f' output, rest')

instance Monad Parser where
    return = pure 
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    Parser p >>= k = Parser $ \input ->
        case p input of
        Left err -> Left err
        Right (output, rest) ->
           let
           Parser p' = k output
           in
           p' rest

instance Alternative Parser where
  -- empty :: Parser a
  empty = Parser $ \_ -> Left "No parser"
  -- (<|>) :: Parser a -> Parser a -> Parser a
  Parser l <|> Parser r = Parser $ \input ->
    case l input of
      Left err ->
        case r input of
          Left err' -> Left err
          Right (output, rest) -> Right (output, rest)
      Right (output, rest) -> Right (output, rest)

charParser :: Char -> Parser Char 
charParser c = Parser $ \input -> 
    case input of 
        (h:rest) | h == c      -> Right (h, rest)
        x        | x == ""     -> Left   "Unexcpected end of input. charParser"
        (h:_)                  -> Left $ "Unexpected " ++ convert [h] ++ ", expected " ++ [c] 
        where convert h = if h == " " then "' '" else h
            
stringParser :: String -> Parser String
stringParser [] = return []
stringParser (x:xs) = charParser x >> stringParser xs >> return (x:xs)

-- natural numbers
natParser :: Parser Int 
natParser = Parser $ \input ->
    let 
        prefix = takeWhile isDigit input
    in
        case prefix of
            [] -> Left "Unexpected end of input. natParser"
            _  -> Right (read prefix, drop (length prefix) input)

intParser :: Parser Int 
intParser = do 
    charParser '('
    charParser '-'
    n <- natParser
    charParser ')'
    return (-n)
    <|> natParser

spaceParser :: Parser String 
spaceParser = do 
    res <- many $ charParser ' '
    return res

-- digits, ws, letters
stringLiteralParser :: Parser String 
stringLiteralParser = Parser $ \input -> 
    let 
        result = takeWhile myPredicate input
        x      = take 1 (drop (length result) input) 
    in 
        case x of 
        "\"" -> Right (result, drop (length result) input) 
        _    -> Left $ "Unexpected string literal " ++ x 
    where 
        myPredicate :: Char -> Bool
        myPredicate c = isAlphaNum c || c == ' ' || c == '_' || c == '-'

stringInQParser :: Parser String 
stringInQParser = do 
    charParser '"'
    str <- stringLiteralParser 
    charParser '"'
    return $ str

dInteger :: Parser Document 
dInteger =  DInteger <$> intParser

dNull :: Parser Document 
dNull = (\_ -> DNull) <$> stringParser "null"

-- can be digits, letters, white spaces
dString :: Parser Document 
dString = DString <$> stringInQParser

emptydString :: Parser Document 
emptydString = do
    stringParser "''"
    return $ DString ("''")

dPrimitiveValue :: Parser Document
dPrimitiveValue = dInteger <|> dNull <|> dString <|> emptydString

listElemParser :: Int -> Parser Document
listElemParser s = do
    stringParser "\n"
    stringParser (take s $ cycle " ")  
    stringParser "- "
    spaceParser 
    doc <- dPrimitiveValue <|> emptyMapP <|> dMapParser (s+2) <|> emptyListParser
    spaceParser
    return doc
    <|> listInListParser (s+2)

listInListParser :: Int -> Parser Document 
listInListParser s = do
    stringParser "\n"
    stringParser (take (s-2) $ cycle " ")
    stringParser "- "
    doc <- some $ listElemParser s -- if this fails than check for dmap
    return $ DList (doc)

listParser :: Int -> Parser Document
listParser s = do
    doc <- some $ listElemParser s
    let doc2 = DList doc
    return doc2

emptyListParser :: Parser Document
emptyListParser = do
    charParser '['
    charParser ']'
    let doc = DList []
    return doc

emptyMapParser :: Parser Document
emptyMapParser = do
    charParser '['
    charParser ']'
    let doc = DMap []
    return doc

mapElemParser :: Int -> Parser (String, Document)
mapElemParser s = do  
    stringParser "\n"
    stringParser (take s $ cycle " ")  
    key <- keyParser  <|> keyParserEmpty 
    stringParser ": "
    spaceParser 
    doc <- dPrimitiveValue <|> emptyListParser <|> listParser (s+2) <|> emptyMapP
    return $ (key, doc)
    <|> mapInMapParser (s+2)

mapInMapParser :: Int -> Parser (String, Document)
mapInMapParser s = do
    stringParser "\n"
    stringParser (take (s-2) $ cycle " ")
    key <- keyParser  <|> keyParserEmpty 
    stringParser ": "
    doc <- some $ mapElemParser s -- if this fails look for a list
    return $ (key, DMap (doc))

dMapParser :: Int -> Parser Document
dMapParser s = do
    doc <- some $ mapElemParser s
    return $ DMap doc

keyParser :: Parser String 
keyParser = Parser $ \input -> 
    let 
        result = takeWhile myPredicate input
        x      = take 1 (drop (length result) input) 
    in 
        case x of 
        ":"  -> Right (result, drop (length result) input) 
        _    -> Left $ "Unexpected literal in key " ++ x 
    where 
        myPredicate :: Char -> Bool
        myPredicate c = isAlphaNum c || c == '_' || c == '-'

keyParserEmpty :: Parser String 
keyParserEmpty  = do
    stringParser "''"
    return "''"

starterParser :: Parser String
starterParser = Parser $ \input -> do
    (a,r) <- runParser (stringParser "---") input
    return (a, r)

emptyMapP :: Parser Document
emptyMapP = do
    charParser '{'
    charParser '}'
    let doc = DMap []
    return doc

parseDoc :: Parser Document 
parseDoc =  do
    _ <- starterParser
    doc <- dPrimitiveValue <|> listParser 0 <|> dMapParser 0 
    return (doc)
    <|> dPrimitiveValue
    <|> emptyListParser 
    <|> emptyMapP

-- IMPLEMENT
-- Change right hand side as you wish
-- You will have to create an instance of FromDocument
data GameStart = GameStart {
    hint_number :: Int,
    occupied_rows :: [Int],
    occupied_cols :: [Int]
} deriving Show

instance FromDocument GameStart where
    fromDocument d = do
        (hints, rowData, colData) <- parseGameStartDocument d
        Right GameStart {
            Lib3.hint_number = hints,
            occupied_rows = rowData,
            occupied_cols = colData
        }

-- This adds game data to initial state
-- Errors are not reported since GameStart is already totally valid adt
-- containing all fields needed
gameStart :: State -> GameStart -> State
gameStart state gs = State {
    rowData = occupied_rows gs,
    colData = occupied_cols gs,
    document = DInteger 1,
    board = replicate 100 Blank,
    Lib1.hint_number = Lib3.hint_number gs
}

-- IMPLEMENT
-- Change right hand side as you wish
-- You will have to create an instance of FromDocument
data Hint = Hint {
    coords :: [(Int, Int)]
} deriving Show

instance FromDocument Hint where
    fromDocument doc = do
        coords <- parseHintDocument doc
        Right Hint {
            coords = coords
        }
    
parseHintDocument :: Document -> Either String [(Int, Int)]
parseHintDocument (DMap [(key, DList l)]) = do
    coords <- getCoord l []
    return coords

-- Adds hint data to the game state
-- Errors are not reported since GameStart is already totally valid adt
-- containing all fields needed
hint :: State -> Hint -> State
hint state h = State {
    rowData = rowData state, 
    colData = colData state, 
    document = document state, 
    board = toggleShipHint (board state) (coords h),
    Lib1.hint_number = Lib1.hint_number state
}


