{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Lib4 (parseDocument) where
import Types (Document(..))

import qualified Data.List as L
import Data.List.Split as S ( splitOn )

import Control.Applicative ((<|>), many) 
import Control.Monad.Trans.State.Strict (State, get, put, runState)
import Control.Monad.Trans.Except ( ExceptT, runExceptT, throwE, catchE) 
import Control.Monad.Trans.Class ( lift ) 
import Data.Char ( isDigit, isAlphaNum, isAlpha ) 
import Data.List(isPrefixOf)

parseDocument :: String -> Either String Document
parseDocument str = 
    case fst $ runParser parseDoc str of 
        Left err  -> Left $ show err
        Right d   -> Right d
    
data ParseError = UnexpectedChar Char
                | UnexpectedString String 
                | NotNumber
                | NotString
                | NotKey
                | UnexpectedEof
                | ParserFailure
                deriving (Eq, Show)
instance Semigroup ParseError where _ <> q = q 
instance Monoid ParseError where mempty = UnexpectedEof 
  
type Parser a = ExceptT ParseError (State String) a 

runParser :: Parser a -> String -> (Either ParseError a, String)
runParser parser = runState (runExceptT parser)

option :: Parser a -> Parser a -> Parser a
option parser1 parser2 = do
  s <- lift get
  parser1 `catchE` \_ -> do
    lift (put s)
    parser2

choice :: [Parser a] -> Parser a
choice = foldr option (throwE ParserFailure)

op :: Parser a -> Parser ()
op p = () <$ p <|> pure ()

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = do
  s <- lift get
  case s of
    x:xs | predicate x -> do lift (put xs)
                             return x
    x:_ -> throwE (UnexpectedChar x)
    []  -> throwE UnexpectedEof

char :: Char -> Parser Char 
char c = satisfy (==c)

nl :: Parser Char 
nl = char '\n'

string :: String -> Parser String 
string inp = do
    str <- lift get 
    let len = length inp 
    let check = inp == take len str 
    if check then
        lift (put (drop len str)) >> return inp  
        else  throwE (UnexpectedString inp)

digits :: Parser Int 
digits = do
    s <- lift get
    let (prefix, rest) = span isDigit s
    case prefix of
        [] -> throwE NotNumber
        _  -> lift (put rest) >> return (read prefix)

literals :: Parser String 
literals = do 
    str <- lift get 
    let (prefix, rest) = span predicate str 
    case prefix of 
         []                              -> throwE NotString 
         (x:_)   |  x == '-'             -> throwE NotString
         (x:y:_) |  x == '-' && y == ' ' -> throwE NotString
         _                               -> lift (put rest) >> return prefix 
    where   
        predicate :: Char -> Bool
        predicate c = isAlphaNum c || c == ' ' || c == '-'

natural :: Parser Int 
natural = digits <* char '\n'

negative :: Parser Int 
negative = fmap negate (char '-' *> natural) 

dint :: Parser Document 
dint = DInteger <$> choice [natural, negative]

dnull :: Parser Document 
dnull = DNull <$ string "null\n"

dstr :: Parser Document 
dstr = DString <$> choice[(string "''" <* nl) >> return "",  (string "\"\"" <* nl) >> return "", 
                          literals <* nl, 
                          char '\'' *> literals <* char '\'' <* nl,
                          char '"' *> literals <* char '"' <* nl] 

value :: Parser Document 
value = dint <|> dnull <|> dstr

emptylist :: Parser Document
emptylist = string "[]\n" >> return(DList [])

emptymap :: Parser Document
emptymap =  string "{}\n" >> return(DMap [])

------------------------------------------------------------
listS :: Parser () 
listS = () <$ (string "- \n" <|> string "-\n" <|> string "- ")

listElemF :: Int -> Parser Document
listElemF i = listS >> doc (i+2) 

listElemSec :: Int -> Parser Document 
listElemSec i = do 
    str <- lift get 
    indent i 
    temp <- lift get 
    if ("- \n" `isPrefixOf` temp) || ("-\n" `isPrefixOf` temp) || ("- " `isPrefixOf` temp)
        then do listS >> doc(i+2)
    else lift (put str) >> throwE ParserFailure

dlist :: Int -> Parser Document
dlist i = emptylist <|> do
    str <- lift get
    op (indent i)
    temp <- lift get
    if "- \n" `isPrefixOf` temp || "-\n" `isPrefixOf` temp || "- " `isPrefixOf` temp
        then do
            fdoc <- listElemF i 
            rdoc <- many $ listElemSec i
            return $ DList $ fdoc:rdoc
    else lift (put str) >> throwE ParserFailure
               
--------------------
keyliterals :: Parser String 
keyliterals = do
    str <- lift get 
    let (prefix, rest) = span keyLiteralPredicate str
    case prefix of 
        []   -> throwE NotKey 
        _    -> lift (put rest) >> return prefix 
    where 
        keyLiteralPredicate :: Char -> Bool
        keyLiteralPredicate c = isAlpha c || c == '_' 

keyCol :: Parser () 
keyCol = () <$ (string ": \n"  <|> string ":\n" <|> string ": ")

keyEmpty :: Parser String 
keyEmpty  = (string "'': \n" <|> string "'':\n" <|> string "'': ") >> return ""

mapkey :: Parser String 
mapkey = choice[keyliterals <* keyCol, char '\'' *> keyliterals <* char '\'' <* keyCol, keyEmpty]

mapElem :: Int -> Parser (String, Document)
mapElem i = do 
    key <- mapkey
    doc <- choice [dlist i, doc(i+2)] 
    return (key, doc)

dmap :: Int -> Parser Document
dmap i = emptymap <|> do 
    fdoc <-  op (indent i) >> mapElem i
    rdoc <-  many ( indent i >> mapElem i) 
    return $ DMap $ fdoc:rdoc 

indent :: Int -> Parser ()
indent n = () <$ string (replicate n ' ')
        
starter :: Parser String
starter =  string "---\n"

doc :: Int -> Parser Document 
doc i = choice [dlist i, dmap i, value]

parseDoc :: Parser Document 
parseDoc = do
    op starter 
    doc 0

----------------------------------

tokens :: String -> [String]
tokens s = L.filter (not . Prelude.null) $ S.splitOn " " s