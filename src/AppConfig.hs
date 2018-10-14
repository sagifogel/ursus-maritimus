{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module AppConfig (loadConfig
                 , ip        
                 , port
                 , getServerConfig
                 , maxQueuedReaderElements
                 , maxQueuedWriterElements
                 ) where

import Data.Yaml
import GHC.Generics
import Control.Monad
import System.Directory

data Config = Config { getStorage :: EventStorageConfig
                     , getServerConfig :: HttpServerConfig 
                     } deriving Show

data EventStorageConfig = EventStorageConfig { maxQueuedReaderElements :: Int
                                             , maxQueuedWriterElements :: Int
                                             } deriving (Show, Generic)

data HttpServerConfig = HttpServerConfig { ip :: String
                                         , port :: Int
                                         } deriving (Show, Generic)

instance FromJSON Config where
  parseJSON (Object v) = Config 
      <$> v .: "eventStorage" 
      <*> v .: "httpServer"

instance FromJSON EventStorageConfig 
                    
instance FromJSON HttpServerConfig

loadConfig :: IO (Either ParseException Config)
loadConfig = getCurrentDirectory >>= (addPath >=> decodeFileEither)

addPath :: String -> IO String 
addPath = return . (++ "/resources/configuration.yaml")