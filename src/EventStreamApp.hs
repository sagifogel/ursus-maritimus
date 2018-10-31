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
import GHC.IO.Handle.Text
import EventReaderStorage
import EventWriterStorage
import Data.ByteString (ByteString)
import qualified Pipes.Prelude as PP
import Control.Monad (unless, (=<<), ap, forever)
import qualified EventReaderStorage as RS
import qualified EventWriterStorage as WS
import Control.Exception (try, throwIO, finally)
import qualified Data.ByteString.Char8 as B (unpack, putStrLn)

data EventStorage = EventStorage { eventStorageConfig :: EventStorageConfig
                                 , eventWriterStorage :: WriterStorage
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
  lift $ return (decodeEither' byteStr :: Either ParseException Event)

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