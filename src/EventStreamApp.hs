{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}                   

module EventStreamApp ( run 
                      , EventStorage(..)
                      ) where 

import Pipes
import Events    
import EventSink
import System.IO
import Data.Yaml
import AppConfig
import Data.Maybe
import System.Process
import Pipes.ByteString
import GHC.IO.Handle.Text
import EventReaderStorage
import EventWriterStorage
import Data.ByteString (ByteString)
import qualified GHC.IO.Exception as G
import Control.Monad (unless, (=<<), ap)
import qualified EventReaderStorage as RS
import qualified EventWriterStorage as WS
import Control.Exception (try, throwIO, finally)
import qualified Data.ByteString.Char8 as B (unpack)

data EventStorage = EventStorage { eventStorageConfig :: EventStorageConfig
                                 , eventWriterStorage :: WriterStorage
                                 , eventReaderStorage :: ReaderStorage }

run :: FilePath -> EventStorageConfig -> ReaderStorage -> WriterStorage -> IO ()
run path config readerStore writerStore = do
                              handle <- handle path 
                              runPipe handle readerStore writerStore
  
runPipe :: Handle -> ReaderStorage -> WriterStorage -> IO ()
runPipe handle readerStore writerStore = 
  finally (runEffect' readerStore writerStore (fromHandle handle)) (hClose handle) 

runEffect' :: ReaderStorage -> WriterStorage -> Proxy X () () ByteString IO () -> IO ()
runEffect' readerStore writerStore = runEffect . (>-> (consume readerStore writerStore))

handle :: FilePath -> IO Handle  
handle path = do
  (_, mOut, _, _) <- createProcess $ (exProc path) { std_out = CreatePipe }
  return $ maybe (error "invalid handle") id mOut

consume :: ReaderStorage -> WriterStorage -> Consumer' ByteString IO ()
consume readerStore writerStore = do
  byteStr <- await 
  let eitherEvent = decodeEither' byteStr
  case eitherEvent of
    Right event -> do
      lift $ RS.put readerStore event
      lift $ WS.put writerStore event
      consume readerStore writerStore
    Left _ -> consume readerStore writerStore
         
exProc :: FilePath -> CreateProcess
exProc = flip proc []