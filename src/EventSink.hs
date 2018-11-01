{-# LANGUAGE RankNTypes #-}

module EventSink ( writeStorageSink
                 , readStorageSink
                 ) where

import Pipes
import Events
import ProcessPipes
import EventWriterStorage
import EventReaderStorage
import Data.ByteString (ByteString)
import qualified EventWriterStorage as WS
import qualified EventReaderStorage as RS

writeStorageSink :: WriterStorage -> Consumer (Maybe Event) IO ()
writeStorageSink writeStorage = putInStorage (WS.put writeStorage) 

readStorageSink :: ReaderStorage -> Consumer (Maybe Event) IO ()
readStorageSink readStorage = putInStorage (RS.put readStorage)

putInStorage :: (Event -> IO ()) -> Consumer (Maybe Event) IO ()
putInStorage f = do
  maybeEvent <- await
  lift $ maybe (return ()) f maybeEvent