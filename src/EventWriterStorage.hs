{-# LANGUAGE InstanceSigs #-}

module EventWriterStorage ( get
                          , put
                          , create
                          , WriterStorage
                          , EventWriterStorage
                          ) where

import Events
import Data.IORef
import Data.DList

newtype WriterStorage = WriterStorage { getStorage :: IORef (DList Event) }

create :: IO WriterStorage
create = do
  ref <- newIORef $ fromList []
  return $ WriterStorage ref

class EventWriterStorage s where
  get :: s -> IO [Event]
  put :: s -> Event -> IO ()
  
instance EventWriterStorage WriterStorage where
  get (WriterStorage ref) = toList <$> readIORef ref
  put (WriterStorage ref) ev = modifyIORef ref (flip snoc ev)  
