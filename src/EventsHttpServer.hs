{-# LANGUAGE OverloadedStrings #-}

module EventsHttpServer (httpService) where

import Web.Scotty
import AppConfig

httpService :: IO ()
httpService = do
  conf <- loadConfig 
  let port' = either (const 3000) (port . getServerConfig) conf
  scotty port' $ root >> eventsRoute
  return () 
  
eventsRoute :: ScottyM ()  
eventsRoute = get "/eventsbytype" $ do
  html "Events counter"
  
root :: ScottyM ()  
root = get "/" $ do
  html "Welcome to the event counter service!"