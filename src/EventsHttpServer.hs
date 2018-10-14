{-# LANGUAGE OverloadedStrings #-}

module EventsHttpServer (httpService) where

import Web.Scotty
import AppConfig

httpService :: IO ()
httpService = do
  conf <- loadConfig
  let p = case conf of 
            Left x -> 3000
            Right conf -> port $ getServerConfig conf
  scotty p $ root >> eventsRoute
  return () 
  
eventsRoute :: ScottyM ()  
eventsRoute = get "/eventsbytype" $ do
  html "Events counter"
  
root :: ScottyM ()  
root = get "/" $ do
  html "Welcome to the event counter service!"