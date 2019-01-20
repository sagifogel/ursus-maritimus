{-# LANGUAGE OverloadedStrings #-}

module EventsHttpServer (httpService) where

import Data.Map
import AppConfig
import Data.Text
import Web.Scotty
import Text.Printf
import EventReaderStorage
import Control.Monad.Trans
import qualified Data.Text.Lazy as T

type EventType = String

httpService :: HttpServerConfig -> ReaderStorage -> IO ()
httpService httpConfig readerStorage =
  scotty (port httpConfig) $ root >> 
    eventsByTypeRoute readerStorage >> 
    eventsByDataRoute readerStorage >> 
    return ()
  
root :: ScottyM ()  
root = get "/" $ do
  html "Welcome to the event counter service!"

eventsByTypeRoute :: ReaderStorage -> ScottyM ()
eventsByTypeRoute readerStorage = get "/eventsbytype" $ do
  serveEvents (getEventCountByType readerStorage) "type"

eventsByDataRoute :: ReaderStorage -> ScottyM ()
eventsByDataRoute readerStorage = get "/eventsbydata" $ do
  serveEvents (getEventCountByData readerStorage) "data"

serveEvents :: IO (Map String Int) -> String -> ActionM ()
serveEvents events fmt = do
  text <- lift $ fmap (foldEvents fmt) events
  html $ T.pack text

foldEvents :: EventType -> Map String Int -> String
foldEvents et = foldrWithKey (\k v acc -> 
  acc ++ printf "Event %s: %s, Count: %s<br/>" et k (show v)) ""