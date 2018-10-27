{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}                   

module EventStreamApp ( run 
                      , EventStorage(..)) where 

import Pipes
import Events    
import EventSink
import System.IO
import AppConfig
import Data.Maybe
import System.Process
import Pipes.ByteString
import EventWriterStorage
import EventReaderStorage
import GHC.IO.Handle.Text
import EventReaderStorage
import Data.ByteString (ByteString)
import qualified GHC.IO.Exception as G
import Control.Monad (unless, (=<<), ap)
import Control.Exception (try, throwIO, finally)
import qualified Data.ByteString.Char8 as BS (unpack)

data EventStorage = EventStorage { eventStorageConfig :: EventStorageConfig
                                 , eventWriterStorage :: WriterStorage
                                 , eventReaderStorage :: ReaderStorage }

run :: FilePath -> EventStorageConfig -> ReaderStorage -> WriterStorage -> IO ()
run path eventStorageConfig eventReaderStorage eventWriterStorage = runPipe (handle path)

runPipe :: IO Handle -> IO ()
runPipe = (ap (finally . runEffect' . fromHandle) hClose =<<)

runEffect' :: Proxy X () () ByteString IO () -> IO ()
runEffect' = runEffect . (>-> stdoutLn)

handle :: FilePath -> IO Handle  
handle path = do
  (_, mOut, _, _) <- createProcess $ (exProc path) { std_out = CreatePipe }
  return $ maybe (error "invalid handle") id mOut

stdoutLn :: Consumer' ByteString IO ()
stdoutLn = do
  str <- await 
  x   <- lift $ try $ putStr $ BS.unpack str
  case x of
    Right () -> stdoutLn  
    Left e@(G.IOError { G.ioe_type = t}) ->
        lift $ unless (t == G.ResourceVanished) $ throwIO e
         
exProc :: FilePath -> CreateProcess
exProc = flip proc []
