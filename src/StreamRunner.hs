{-# LANGUAGE QuasiQuotes #-}

module StreamRunner (stream)  where

import AppConfig
import Text.Printf
import System.Directory
import EventsHttpServer
import qualified EventStreamApp
import Control.Concurrent.Async
import qualified EventWriterStorage
import qualified EventReaderStorage

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
  let storageConfig = getStorageConfig config
  let httpConfig = getServerConfig config

  eventWriter <- EventWriterStorage.create
  eventReader <- EventReaderStorage.create
  http <- async $ httpService httpConfig eventReader
  app <- async $ EventStreamApp.run dataGen storageConfig eventReader eventWriter
  waitBoth app http
  return ()

defaultConfig :: Config
defaultConfig = Config (HttpServerConfig "0.0.0.0" 8080) (EventStorageConfig 2048 240)  

