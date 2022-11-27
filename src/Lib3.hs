{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Lib3 (parseDocument, hint, gameStart, parseDocument, GameStart, Hint) where

import Types ( Document(..), FromDocument, fromDocument )
import Lib1 (State(..))
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
        (input)  | input == "" -> Left   "Unexcpected end of input. charParser"
        (h:rest)               -> Left $ "Unexpected " ++ convert [h] ++ ", expected " ++ [c] 
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
    charParser '-'
    n <- natParser
    return (-n)
    <|> natParser

spaceParser :: Parser String 
spaceParser = do 
    res <- (many $ charParser ' ')
    return (res)

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
        myPredicate c = isAlphaNum c || c == ' '

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

dPrimitiveValue :: Parser Document
dPrimitiveValue = dInteger <|> dNull <|> dString

listElemParser :: Int -> Parser Document
listElemParser s = do
    stringParser "\n"
    stringParser (take s $ cycle " ")  
    stringParser "- "
    spaceParser 
    doc <- dPrimitiveValue <|> emptyMapParser <|> dMapParser (s+2)
    spaceParser
    return $ (doc)
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
    let document = DList (doc)
    return $ (document)

emptyListParser :: Parser Document
emptyListParser = do
    charParser '['
    charParser ']'
    let doc = DList ([])
    return $ (doc)

emptyMapParser :: Parser Document
emptyMapParser = do
    charParser '['
    charParser ']'
    let doc = DMap ([])
    return $ (doc)

mapElemParser :: Int -> Parser (String, Document)
mapElemParser s = do  
    stringParser "\n"
    stringParser (take s $ cycle " ")  
    key <- keyParser s <|> keyParserEmpty s
    stringParser ": "
    spaceParser 
    doc <- dPrimitiveValue <|> emptyListParser <|> listParser (s+2)
    return $ (key, doc)
    <|> mapInMapParser (s+2)

mapInMapParser :: Int -> Parser (String, Document)
mapInMapParser s = do
    stringParser "\n"
    stringParser (take (s-2) $ cycle " ")
    key <- keyParser s <|> keyParserEmpty s
    stringParser ": "
    doc <- some $ mapElemParser s -- if this fails look for a list
    return $ (key, DMap (doc))

dMapParser :: Int -> Parser Document
dMapParser s = do
    doc <- some $ mapElemParser s
    return $ DMap (doc)

keyParser :: Int -> Parser String 
keyParser s = Parser $ \input -> 
    let 
        result = takeWhile myPredicate input
        x      = take 1 (drop (length result) input) 
    in 
        case x of 
        ":"  -> Right (result, drop (length result) input) 
        _    -> Left $ "Unexpected literal in key " ++ x 
    where 
        myPredicate :: Char -> Bool
        myPredicate c = isAlphaNum c || c == '_'

keyParserEmpty :: Int -> Parser String 
keyParserEmpty s = do
    charParser '"' 
    charParser '"'
    return ("\"\"")

starterParser :: Parser String
starterParser = Parser $ \input -> do
    (a,r) <- runParser (stringParser "---") input
    return (a, r)

parseDoc :: Parser Document 
parseDoc =  do
    sp <- starterParser
    doc <- dPrimitiveValue <|> listParser 0 <|> dMapParser 0
    return (doc)

-- IMPLEMENT
-- Change right hand side as you wish
-- You will have to create an instance of FromDocument
data GameStart = GameStart deriving Show

-- This adds game data to initial state
-- Errors are not reported since GameStart is already totally valid adt
-- containing all fields needed
gameStart :: State -> GameStart -> State
gameStart (State l) d = State $ ("Game started: " ++ show d) : l

-- IMPLEMENT
-- Change right hand side as you wish
-- You will have to create an instance of FromDocument
data Hint = Hint deriving Show

-- Adds hint data to the game state
-- Errors are not reported since GameStart is already totally valid adt
-- containing all fields needed
hint :: State -> Hint -> State
hint (State l) h = State $ ("Hint " ++ show h) : l
