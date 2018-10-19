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
  toJSON (Event eventType _data timestamp) = 
    object ["data" .= _data,
            "eventType" .= eventType, 
            "timestamp" .= timestamp]
                   