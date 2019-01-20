{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}                   

module EventStreamApp ( run 
                      , EventStorage(..)
                      ) where 

import Pipes
import Events    
import System.IO
import Data.Yaml
import EventSink
import AppConfig
import Data.Maybe
import System.Process
import Pipes.ByteString
import EventReaderStorage
import EventWriterStorage
import Control.Monad (forever)
import Control.Exception (finally)
import Data.ByteString (ByteString)
import qualified EventReaderStorage as RS
import qualified EventWriterStorage as WS

data EventStorage = EventStorage { eventWriterStorage :: WriterStorage
                                 , eventReaderStorage :: ReaderStorage }

run :: FilePath -> EventStorage -> IO ()
run path eventStorage = do
  let reader = eventReaderStorage eventStorage
  let writer = eventWriterStorage eventStorage

  handle <- processHandle path
  finally (runEffect $ effect handle reader writer) (hClose handle)

effect :: Handle -> ReaderStorage -> WriterStorage -> Effect IO ()
effect handle readerStore writerStore = 
  producer handle >-> consumer readerStore writerStore

producer :: Handle -> Producer ByteString IO ()
producer = fromHandle

consumer :: ReaderStorage -> WriterStorage -> Consumer ByteString IO () 
consumer readerStore writerStore =
  forever $ consume >~ collect >~ do
    writeStorageSink writerStore
    readStorageSink readerStore
    
consume :: Consumer ByteString IO (Either ParseException Event)
consume = do 
  byteStr <- await
  lift $ return $ decodeEither' byteStr

collect :: Consumer (Either ParseException Event) IO (Maybe Event)
collect = do 
  parsedEvent <- await
  lift $ return $ either (const Nothing) Just parsedEvent
  
processHandle :: FilePath -> IO Handle  
processHandle path = do
  (_, mOut, _, _) <- createProcess $ (exProc path) { std_out = CreatePipe }
  return $ maybe (error "invalid handle") id mOut
         
exProc :: FilePath -> CreateProcess
exProc = flip proc []