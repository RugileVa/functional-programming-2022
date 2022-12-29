{-# LANGUAGE OverloadedStrings #-}


import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class(lift)
import Control.Monad.Trans.State.Strict
    ( evalStateT, get, modify, put, StateT )
import Data.ByteString as B ( empty, ByteString )
import Data.Either as E (fromRight)
import qualified Data.List as L
import Data.Text as T ( concat, drop, pack, unpack, Text )
--import Data.Text.Encoding.Base64 (decodeBase64)
import Data.Text.IO as TIO ( hPutStrLn, putStrLn )
import Data.List.Split as S ( splitOn )
import Data.Char (isSpace)
import Lib2 ( renderDocument )
import Lib4 ( parseDocument )
import Network.Wreq ( post, postWith, defaults, header, responseBody, statusCode )
import qualified Network.Wreq as Wreq

import Control.Lens
import System.Console.Repline
  ( CompleterStyle (Word),
    ExitDecision (Exit),
    HaskelineT,
    WordCompleter,
    evalRepl,
  )

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (stderr)

import Data.String.Conversions

type Repl a = HaskelineT (StateT (String) IO) a

commandShow :: String
commandShow = "show"

commandToggle :: String
commandToggle = "toggle"

commandGetId :: String
commandGetId = "getGameId"

cmd :: String -> Repl ()
cmd c
  | trim c == commandGetId = registerNewGame
  | trim c == commandShow = makeShowRequest 
  | commandToggle L.isPrefixOf trim c = do
    case tokens c of
      [_] -> liftIO $ Prelude.putStrLn $ "Illegal format, command \"" ++ commandToggle ++ "\" expects x y pair as argument"
      t   -> makeToggleRequest (L.drop 1 t) 
cmd c = liftIO $ Prelude.putStrLn $ "Unknown command: " ++ c

tokens :: String -> [String]
tokens s = L.filter (not . Prelude.null) $ S.splitOn " " s

trim :: String -> String
trim = f . f
  where f = L.reverse . L.dropWhile isSpace

-----------------------------------------------------------
registerNewGame :: Repl()
registerNewGame = do 
  url <- lift get 
  case url of 
    x | x /= "http://localhost:3000/new" -> liftIO $ Prelude.putStrLn $ "You cannot register a new game, because you already have a game ID"
    _  -> do 
      resp <- liftIO $ Wreq.get (url ++ "/getGameId")
      let respID = cs (resp ^. responseBody)
      lift $ modify (\(u) -> ("http://localhost:3000/" ++ respID))
      liftIO $ Prelude.putStrLn $ "Your game ID is " ++ respID 

makeShowRequest :: Repl()
makeShowRequest = do 
  url  <- lift get
  resp <- liftIO $ Wreq.get (url ++ "/show")
  let respDoc = Lib.parseDocument (cs (resp ^. responseBody))
  case (respDoc :: Either String Document) of
    Right (DString m) -> liftIO $ Prelude.putStrLn $ m
    Right doc -> do
      let currGameState = fromDocument doc
      case (currGameState :: Either String State) of
        Right st    -> liftIO $ Prelude.putStrLn $ render st
        Left  err   -> liftIO $ fatal $ cs err
    Left msg -> liftIO $ fatal $ cs msg

makeToggleRequest :: [String] -> Repl()
makeToggleRequest [r,c] = do 
  url <- lift get
  let opts = defaults & header "Content-type" .~ ["text/x-yaml"]
  let body = cs $ renderDocument $ DString (r ++ " " ++ c) :: B.ByteString
  resp <- liftIO $ postWith opts (url ++ "/toggle") body
  let doc = Lib.parseDocument $ cs (resp ^. responseBody)
  case (doc :: Either String Document) of
    Right (DString d) -> liftIO $ Prelude.putStrLn d 
    Left _            -> liftIO $ Prelude.putStrLn "Parsing error"    
makeToggleRequest _ = liftIO $ Prelude.putStrLn $ "toggle command expects x y as argument"    

-----------------------------------------------------------------
completer :: Monad m => WordCompleter m
completer n = do
  let names = [commandShow, commandToggle]
  return $ Prelude.filter (L.isPrefixOf n) names

ini :: Repl ()
ini = do
  liftIO $ TIO.putStrLn "Welcome to Bimaru v4.\nIf you are playing a new game, you must first get you game ID by writing getGameId into the console.\nLater press [TAB] for available commands list."

fatal :: Text -> IO ()
fatal msg = do
  TIO.hPutStrLn stderr $ T.concat ["ERROR: ", msg]
  exitFailure

final :: Repl ExitDecision
final = do
  liftIO $ TIO.putStrLn "Goodbye!"
  return Exit

main :: IO ()
main = do
  args <- getArgs
  case args of
    [token] -> run $ T.pack token
    _ -> fatal "token not provided, expected at least one command argument"

run :: T.Text -> IO ()
run token = do
  let url = "localhost:3000"    
  let fullUrl = T.unpack (T.concat ["http://", url, "/", token])
  evalStateT (evalRepl (const $ pure ">>> ") cmd [] Nothing Nothing (Word completer) ini final) (fullUrl)