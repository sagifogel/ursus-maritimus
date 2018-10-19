{-# LANGUAGE OverloadedStrings #-}

module EventsHttpServer (httpService) where

import Data.Map
import AppConfig
import Data.Text
import Web.Scotty
import Text.Printf
import EventReaderStorage
import Control.Monad.Trans
import Data.String (fromString)

httpService :: ReaderStorage -> IO ()
httpService readerStorage = do
  conf <- loadConfig 
  let port' = either (const 3000) (port . getServerConfig) conf
  scotty port' $ root >> eventsByTypeRoute readerStorage >> eventsByDataRoute readerStorage
  return () 
  
root :: ScottyM ()  
root = get "/" $ do
  html "Welcome to the event counter service!"

eventsByTypeRoute :: ReaderStorage -> ScottyM ()  
eventsByTypeRoute readerStorage = get "/eventsbytype" $ do
  let events = getEventCountByType readerStorage
  text <- lift $ fmap (foldEvents "Event type: %s, Count: %s") events
  html $ fromString text 

eventsByDataRoute :: ReaderStorage -> ScottyM ()  
eventsByDataRoute readerStorage = get "/eventsbydata" $ do
  let events = getEventCountByData readerStorage
  text <- lift $ fmap  (foldEvents "Event data: %s, Count: %s") events
  html $ fromString text

foldEvents :: String -> Map String Int -> String
foldEvents fmt = foldrWithKey (\k v acc -> printf fmt k (show v)) ""