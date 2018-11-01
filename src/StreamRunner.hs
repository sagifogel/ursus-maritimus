{-# LANGUAGE QuasiQuotes #-}

module StreamRunner (stream)  where

import Pipes
import AppConfig
import Text.Printf
import EventStreamApp
import System.Directory
import EventsHttpServer
import Control.Concurrent.Async
import Data.ByteString (ByteString)
import qualified EventWriterStorage
import qualified EventReaderStorage
import qualified EventStreamApp as App

stream :: [String] -> IO ()
stream args = 
  case args of
    [] -> putStrLn "Invalid number of parameters to event stream runner.\n\
          \Please supply the data generator location via a parameter in the command line."
    (dataGen: _) -> do
      doesFileExist dataGen >>= \exists ->
        if exists then bootstrapStream dataGen
        else do 
          putStrLn $ printf "Data generator path \"%s\" is invalid.\n\ 
             \Please make sure the file exists at the specified location." dataGen
          return ()

bootstrapStream :: String -> IO ()          
bootstrapStream dataGen = do
  loadedConfig <- loadConfig
  let config = either (const defaultConfig) id loadedConfig
  let httpConfig = getServerConfig config

  eventWriter <- EventWriterStorage.create
  eventReader <- EventReaderStorage.create
  http <- async $ httpService httpConfig eventReader
  app <- async $ App.run dataGen (EventStorage eventWriter eventReader)
  waitBoth app http
  wait app

defaultConfig :: Config
defaultConfig = Config (HttpServerConfig 8080)