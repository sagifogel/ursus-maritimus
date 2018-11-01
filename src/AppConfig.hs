{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module AppConfig ( loadConfig
                 , Config(..)
                 , HttpServerConfig(..)
                 ) where

import Data.Yaml
import GHC.Generics
import Control.Monad
import System.Directory

newtype Config = Config { getServerConfig :: HttpServerConfig } deriving Show
newtype HttpServerConfig = HttpServerConfig { port :: Int } deriving (Show, Generic)

instance FromJSON Config where
  parseJSON (Object v) = Config
      <$> v .: "httpServer"

instance FromJSON HttpServerConfig

loadConfig :: IO (Either ParseException Config)
loadConfig = getCurrentDirectory >>= (addPath >=> decodeFileEither)

addPath :: String -> IO String 
addPath = return . (++ "/resources/configuration.yaml")