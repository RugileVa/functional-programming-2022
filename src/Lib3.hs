{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Lib3 (parseDocument, hint, gameStart, GameStart, Hint) where
import Lib2 (parseGameStartDocument, checkKey, getCoord, toggleShipHint)
import Types ( Document(..), FromDocument, fromDocument )
import Lib1 (State(..), Cell(..))
import Control.Applicative
import Control.Monad
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
          Left _ -> Left err
          Right (output, rest) -> Right (output, rest)
      Right (output, rest) -> Right (output, rest)

optionalP :: Parser a -> Parser ()
optionalP p = () <$ p <|> pure ()

charParser :: Char -> Parser Char 
charParser c = Parser $ \input -> 
    case input of 
        (h:rest) | h == c      -> Right (h, rest)
        x        | x == ""     -> Left   "Unexcpected end of input in charParser"
        (h:_)                  -> Left $ "Unexpected " ++ convert [h] ++ ", expected " ++ [c] 
        where convert h = if h == " " then "' '" else h
            
stringParser :: String -> Parser String
stringParser [] = return []
stringParser (x:xs) = charParser x >> stringParser xs >> return (x:xs)

spaceParser :: Parser String 
spaceParser = do many $ charParser ' '

nl :: Parser String 
nl = stringParser "\n"

natParser :: Parser Int 
natParser = Parser $ \input ->
    let 
        prefix = takeWhile isDigit input
    in
        case prefix of
            [] -> Left "Unexpected end of input in natParser"
            _  -> Right (read prefix, drop (length prefix) input)

intParser :: Parser Int 
intParser = do 
    optionalP (charParser '(')
    charParser '-'
    n <- natParser
    optionalP (charParser ')')
    nl 
    return (-n)
    <|> do
    n <- natParser
    nl 
    return n

-- digits, ws, letters, '-'
stringLiteralParser :: Parser String 
stringLiteralParser = Parser $ \input -> 
    let 
        result = takeWhile myPredicate input
    in 
        case result of 
        [] -> Left $ "Unexpected end of input in string"  
        _  -> Right (result, drop (length result) input)
    where 
        myPredicate :: Char -> Bool
        myPredicate c = isAlphaNum c || c == ' ' || c == '-'

stringInQParser :: Parser String 
stringInQParser = do 
    optionalP (stringParser "\"" <|> stringParser "'") 
    str <- stringLiteralParser 
    optionalP (stringParser "\"" <|> stringParser "'") 
    nl 
    return str

emptydString :: Parser Document 
emptydString = (stringParser "'" >> stringParser "'") <|> stringParser "\"\"" >> nl >> return(DString "")

dInteger :: Parser Document 
dInteger =  DInteger <$> intParser

dNull :: Parser Document 
dNull = DNull <$ stringParser "null\n"

dString :: Parser Document 
dString = DString <$> stringInQParser <|> emptydString 

dPrimitiveValue :: Parser Document
dPrimitiveValue = dInteger <|> dNull <|> dString 

docParser :: Int -> Parser Document 
docParser s = listParser s <|> dMapParser s <|> dPrimitiveValue 

emptyListP :: Parser Document
emptyListP = stringParser "[]" >> nl >> return(DList [])

emptyMapP :: Parser Document
emptyMapP = stringParser "{}" >> nl >> return(DMap [])

listS :: Parser () 
listS = () <$ (stringParser "-" >> (stringParser " \n" <|> stringParser "\n" <|> stringParser " ")) 

listElemParser :: Int -> Parser Document
listElemParser s = do
    listS
    docParser (s+2)

listParser :: Int -> Parser Document
listParser s = emptyListP <|> do
    fdoc <- optionalP (indent s) >> listElemParser s
    rdoc <- many ( indent s  >> listElemParser s )
    return $ DList $ fdoc:rdoc

keyCol :: Parser () 
keyCol = () <$ (stringParser ":" >> (stringParser " \n" <|> stringParser "\n" <|> stringParser " "))

keyParser :: Parser String 
keyParser = Parser $ \input -> 
    let  
        result = takeWhile myPredicate input
    in 
        case result of 
        []   -> Left "Empty key" 
        _    -> Right (result, drop (length result) input) 
    where 
        myPredicate :: Char -> Bool
        myPredicate c = isAlpha c || c == '_' || c == '-'

keyParserEmpty :: Parser String 
keyParserEmpty  = stringParser "''" >> return ""

mapElemParser :: Int -> Parser (String, Document)
mapElemParser s = do 
    optionalP (charParser '\'' )
    key <- keyParser <|> keyParserEmpty
    optionalP (charParser '\'' ) <* keyCol
    doc <- listParser s <|> docParser (s+2) 
    return (key, doc)

dMapParser :: Int -> Parser Document
dMapParser s = emptyMapP <|> do
    fdoc <- optionalP (indent s) >> mapElemParser s
    rdoc <- many (indent s >> mapElemParser s)
    return $ DMap $ fdoc:rdoc

indent :: Int -> Parser ()
indent n = replicateM_ n (charParser ' ')

starterParser :: Parser String
starterParser =  stringParser "---\n"

parseDoc :: Parser Document 
parseDoc =  optionalP starterParser >>
    listParser 0 <|> dMapParser 0 <|> dPrimitiveValue

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
        (hints, rowDat, colDat) <- parseGameStartDocument d
        Right GameStart {
            Lib3.hint_number = hints,
            occupied_rows = rowDat,
            occupied_cols = colDat
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
        coord <- parseHintDocument doc
        Right Hint {
            coords = coord
        }
    
parseHintDocument :: Document -> Either String [(Int, Int)]
parseHintDocument (DMap [(_, DList l)]) = do getCoord l []

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


