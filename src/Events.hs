{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}                   

module Events where

import Data.Yaml
import GHC.Generics

data Event = Event { _data :: String
                   , eventType :: String
                   , timestamp :: Integer
                   } deriving (Show, Generic)

instance FromJSON Event 
                    
instance ToJSON Event where         
  toJSON (Event eventType value timestamp) = 
    object ["eventType" .= eventType,
            "_data" .= data_, 
            "timestamp" .= timestamp]
                   