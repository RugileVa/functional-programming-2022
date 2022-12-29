{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Main where

import Web.Spock
import Web.Spock.Config
import Data.IORef
import Data.Text (Text, pack, unpack)
import Data.HashMap.Strict as Map
import GHC.Conc (TVar, newTVarIO, readTVarIO, atomically, STM, writeTVar, readTVar, retry)
import Lib1 (toggle, State (..), Cell(..))
import Lib2 (renderDocument)
import Lib4 (parseDocument, tokens)
import Data.UUID.V4 ( nextRandom)
import Data.UUID (toText, toString, UUID)
import Control.Monad.Cont (MonadIO(liftIO))
import qualified Data.List as L
import Data.List.Split as S ( splitOn )

import Data.String.Conversions


data ServerState = ServerState {games :: TVar (HashMap String State)}

        -- SpockM conn sess globalstate a 
type Api = SpockM () () ServerState ()

app :: Api
app = do
    get ("new/getGameId") $ do    -- uzregint nauja zaidima
        st <- getState
        id <- liftIO nextRandom   
        _  <- liftIO $ atomically $ insertNewGame st id
        text(toText id) 

    get (var <//> "show") $ \(gameId :: String) -> do 
        gamestate <- getState >>= (liftIO . readTVarIO . games)
        case Map.lookup gameId gamestate of 
            Nothing  -> text (pack (renderDocument (DString "Game ID not found")))
            Just st  -> text (pack (renderDocument (toDocument st)))

    post (var <//> "toggle") $ \(gameId :: String) -> do  
        gamestate <- getState >>= (liftIO . readTVarIO . games)
        case Map.lookup gameId gamestate of 
            Nothing  -> text (pack (renderDocument (DString "Game ID not found")))
            Just st  -> do 
                    b <- body
                    case parseDocument $ cs b of
                       Left e -> text (pack (renderDocument (DString e)))
                       Right (DString k) -> do 
                                let tok = tokens k 
                                st <- getState
                                liftIO $ atomically $ toggleGame st gameId tok
                                stnew <- getState >>= (liftIO . readTVarIO . games)
                                case Map.lookup gameId stnew of 
                                    Nothing -> text (pack (renderDocument (DString "Game ID not found"))) 
                                    Just gs -> text $ pack $ (renderDocument (DString (hasSolved gs answer)))

insertNewGame :: ServerState -> UUID -> STM ()
insertNewGame st id = do 
    currst <- readTVar (games st)
    let newst = insert (toString id) initialstate currst 
    writeTVar (games st) newst

toggleGame :: ServerState -> String -> [String] -> STM ()
toggleGame st id tok = do 
    currst <- readTVar (games st) 
    case Map.lookup id currst of 
        Nothing -> retry
        Just gs -> do  
           let toggledst = toggle gs tok 
           let newst = insert id toggledst currst
           writeTVar (games st) newst
    
main :: IO ()
main = do
    st   <- ServerState <$> newTVarIO empty
    cnfg <- defaultSpockCfg () PCNoDatabase st
    runSpock 3000 (spock cnfg app) -- runSpockNoBanner

initialstate :: State 
initialstate = State {rowData = [2, 0, 2, 2, 2, 0, 6, 0, 3, 3], colData = [1, 1, 2, 3, 1, 4, 2, 4, 2, 0], board = replicate 100 Blank, document = DNull, hint_number = 0}

answer :: State 
answer = State {rowData = [2, 0, 2, 2, 2, 0, 6, 0, 3, 3], colData = [1, 1, 2, 3, 1, 4, 2, 4, 2, 0],  
                board = [Blank, Blank, Ship,  Blank, Blank, Ship,  Blank, Blank, Blank, Blank, 
                         Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank,
                         Blank, Blank, Blank, Ship,  Blank, Blank, Blank, Ship,  Blank, Blank,
                         Blank, Blank, Blank, Ship,  Blank, Blank, Blank, Ship,  Blank, Blank,
                         Blank, Blank, Blank, Blank, Blank, Ship,  Blank, Ship,  Blank, Blank,
                         Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank,
                         Blank, Blank, Ship,  Ship,  Blank, Ship,  Ship,  Ship,  Ship,  Blank,
                         Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank,
                         Ship,  Ship,  Blank, Blank, Blank, Blank, Blank, Blank, Ship,  Blank,
                         Blank, Blank, Blank, Blank, Ship,  Ship,  Ship,  Blank, Blank, Blank], document = DNull, hint_number = 0}

hasSolved :: State -> State -> String
hasSolved st1 st2 = if st1 == st2 then "Well done - you have solved the bimaru puzzle" else "The cell was toggled - try again"

-- toggle 3 1 
-- toggle 6 1 
-- toggle 4 3 
-- toggle 8 3 
-- toggle 4 4 
-- toggle 8 4 
-- toggle 6 5 
-- toggle 8 5 
-- toggle 3 7 
-- toggle 4 7 
-- toggle 6 7 
-- toggle 7 7 
-- toggle 8 7
-- toggle 9 7
-- toggle 1 9 
-- toggle 2 9 
-- toggle 9 9 
-- toggle 5 10 
-- toggle 6 10 
-- toggle 7 10