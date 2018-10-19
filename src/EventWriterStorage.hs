{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module EventWriterStorage (get) where

import Events
import Data.DList

newtype WriterStorage = WriterStorage { getStorage :: DList Event }

class EventWriterStorage s where
  get :: s -> [Event]
  put :: s -> Event -> s
  
instance EventWriterStorage WriterStorage  where
  get = toList . getStorage
  put = (WriterStorage .) . snoc . getStorage  
