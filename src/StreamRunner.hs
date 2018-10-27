{-# LANGUAGE QuasiQuotes #-}

module StreamRunner (stream)  where

import AppConfig
import Text.Printf
import System.Directory
import EventsHttpServer
import qualified EventWriterStorage
import qualified EventReaderStorage
import qualified EventStreamApp

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
  let eventWriter = EventWriterStorage.create
  let storageConfig = getStorageConfig config
  
  eventReader <- EventReaderStorage.create
  httpService eventReader
  EventStreamApp.run dataGen storageConfig eventReader eventWriter

defaultConfig :: Config
defaultConfig = Config (HttpServerConfig "0.0.0.0" 8080) (EventStorageConfig 2048 240)  

