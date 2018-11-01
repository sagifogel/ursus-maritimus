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

httpService :: HttpServerConfig -> ReaderStorage -> IO ()
httpService httpConfig readerStorage = do
  scotty (port httpConfig) $ root >> eventsByTypeRoute readerStorage >> eventsByDataRoute readerStorage
  return ()
  
root :: ScottyM ()  
root = get "/" $ do
  html "Welcome to the event counter service!"

eventsByTypeRoute :: ReaderStorage -> ScottyM ()
eventsByTypeRoute readerStorage = get "/eventsbytype" $ do
  serveEvents (getEventCountByType readerStorage) "Event type: %s, Count: %s<br/>"

eventsByDataRoute :: ReaderStorage -> ScottyM ()
eventsByDataRoute readerStorage = get "/eventsbydata" $ do
  serveEvents (getEventCountByData readerStorage) "Event data: %s, Count: %s<br/>"

serveEvents :: IO (Map String Int) -> String -> ActionM ()
serveEvents events fmt = do
  text <- lift $ fmap (foldEvents fmt) events
  html $ T.pack text

foldEvents :: String -> Map String Int -> String
foldEvents fmt = foldrWithKey (\k v acc -> acc ++ printf fmt k (show v)) ""