{-# LANGUAGE OverloadedStrings #-}                   

module Events (Event(..)) where

import Data.Yaml
import GHC.Generics

data Event = Event { _data :: String
                   , eventType :: String
                   , timestamp :: Integer
                   } deriving Show

instance FromJSON Event where
  parseJSON (Object v) = Event 
      <$> v .: "data" 
      <*> v .: "eventType"
      <*> v .: "timestamp"
                    
instance ToJSON Event where         
  toJSON (Event eventType _data timestamp) = 
    object ["data" .= _data,
            "eventType" .= eventType, 
            "timestamp" .= timestamp]
                   