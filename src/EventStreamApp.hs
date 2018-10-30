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
import Control.Monad (unless, (=<<), ap)
import qualified EventReaderStorage as RS
import qualified EventWriterStorage as WS
import Control.Exception (try, throwIO, finally)
import qualified Data.ByteString.Char8 as B (unpack)

import Data.Map as Map
import Data.String

data EventStorage = EventStorage { eventStorageConfig :: EventStorageConfig
                                 , eventWriterStorage :: WriterStorage
                                 , eventReaderStorage :: ReaderStorage }

run :: FilePath -> EventStorage -> IO (Handle, Effect IO ())
run path eventStorage = do
  handle <- handle path
  let pipe = runPipe handle (eventReaderStorage eventStorage) (eventWriterStorage eventStorage)
  return (handle, pipe)

runPipe :: Handle -> ReaderStorage -> WriterStorage -> Effect IO ()
runPipe handle readerStore writerStore = 
  (fromHandle handle) >-> compose readerStore writerStore

compose :: ReaderStorage -> WriterStorage -> Consumer ByteString IO ()
compose readerStore writerStore = 
  consume >~ collect >~ do 
      writeStorageSink writerStore
      readStorageSink readerStore

consume :: Consumer ByteString IO (Either ParseException Event)
consume = do 
  byteStr <- await
  return $ decodeEither' byteStr 

collect :: Consumer (Either ParseException Event) IO (Maybe Event)
collect = do 
  parsedEvent <- await
  return $ either (const Nothing) Just parsedEvent

handle :: FilePath -> IO Handle  
handle path = do
  (_, mOut, _, _) <- createProcess $ (exProc path) { std_out = CreatePipe }
  return $ maybe (error "invalid handle") id mOut
         
exProc :: FilePath -> CreateProcess
exProc = flip proc []