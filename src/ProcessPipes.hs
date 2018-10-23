{- copied from pipes-process repo @https://github.com/stepcut/pipes-process -}
{-# LANGUAGE RankNTypes, FlexibleContexts, TypeFamilies #-}

module ProcessPipes ( PipesProcess
                    , closeStdin
                    , withProcess
                    , readProcess
                    , writeProcess
                    , flushProcess
                    , start
                    , stop ) where

import Pipes
import System.Exit (ExitCode)
import Data.Maybe (catMaybes)
import Control.Monad.Catch (MonadCatch)
import System.IO (hClose, hIsEOF, hFlush)
import Control.Concurrent.STM (atomically)
import Data.ByteString (ByteString, hGetSome, hPut)
import Pipes.Safe (Base, MonadSafe, bracket, finally)
import Control.Exception (SomeException, catch, throw)
import Control.Concurrent (ThreadId, killThread, forkIO)
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVar, putTMVar, takeTMVar)
import System.Process (StdStream(..), CreateProcess(..), ProcessHandle(..), createProcess, waitForProcess, terminateProcess, getProcessExitCode)

data PipesProcess = PipesProcess { action         :: TMVar OutAction
                                 , input          :: TMVar InAction
                                 , processHandle  :: ProcessHandle
                                 , tids           :: [ThreadId]
                                 }

data OutAction
    = Stdout ByteString
    | Stderr ByteString
    | Terminated ExitCode
    | ExceptionRaised SomeException
      deriving Show

data InAction
    = Stdin ByteString
    | Flush
    | CloseStdin
      deriving Show

withProcess :: (MonadSafe m, Base m ~ IO) => CreateProcess -> (PipesProcess -> m r) -> m r
withProcess createProcess = bracket (start createProcess) stop

start :: CreateProcess -> IO PipesProcess
start cp =
    do action <- atomically newEmptyTMVar
       outEOF <- atomically newEmptyTMVar
       errEOF <- atomically newEmptyTMVar
       input  <- atomically newEmptyTMVar
       (minh, mouth, merrh, proch) <- createProcess (cp { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe })
       inTid <- case minh of
                  Nothing -> return Nothing
                  (Just inh) ->
                      do tid <- forkIO $ let loop = do inaction <- atomically $ takeTMVar input
                                                       case inaction of
                                                         (Stdin bs) ->
                                                             do hPut inh bs
                                                                loop
                                                         Flush ->
                                                             do hFlush inh
                                                                loop
                                                         (CloseStdin) ->
                                                             do hFlush inh
                                                                hClose inh
                                         in loop
                         return (Just tid)
       outTid <- case mouth of
                   Nothing -> return Nothing
                   (Just outh) ->
                       do outTid <- forkIO $ let loop = do b <- hGetSome outh defaultChunkSize
                                                           atomically $ putTMVar action (Stdout b)
                                                           eof <- hIsEOF outh
                                                           if eof
                                                             then atomically $ putTMVar outEOF ()
                                                             else loop
                                             in loop `catch` (\e -> do atomically $ putTMVar action (ExceptionRaised e))
                          return (Just outTid)

       errTid <- case merrh of
                   Nothing -> return Nothing
                   (Just errh) ->
                       do errTid <- forkIO $ let loop = do b <- hGetSome errh 100
                                                           atomically $ putTMVar action (Stderr b)
                                                           eof <- hIsEOF errh
                                                           if eof
                                                             then atomically $ putTMVar errEOF ()
                                                             else loop
                                             in loop `catch` (\e -> do atomically $ putTMVar action (ExceptionRaised e))
                          return (Just errTid)
       termTid <- forkIO $
                     (do atomically $ takeTMVar outEOF
                         atomically $ takeTMVar errEOF
                         ec <- waitForProcess proch
                         atomically $ putTMVar action (Terminated ec))
                     `catch` (\e -> do atomically $ putTMVar action (ExceptionRaised e))

       return $ PipesProcess action input proch (termTid : (catMaybes [inTid, outTid, errTid]))

stop :: PipesProcess -> IO ()
stop pipesProcess =
        do atomically $ putTMVar (input pipesProcess) CloseStdin
           let proch = processHandle pipesProcess
           mec <- getProcessExitCode proch
           case mec of
             Nothing ->
                 do
                    terminateProcess proch
                    _ <- waitForProcess proch
                    return ()
             (Just _) -> return ()
           mapM_ killThread (tids pipesProcess)

readProcess :: (MonadIO m) => PipesProcess -> Producer' (Either ByteString ByteString) m ExitCode
readProcess pipesProcess = loop where
    loop = do a <- lift $ liftIO $ atomically $ takeTMVar (action pipesProcess)
              case a of
                (Stdout b) ->
                    do yield (Right b)
                       loop
                (Stderr b) ->
                    do yield (Left b)
                       loop
                (Terminated ec) ->
                    do return ec
                (ExceptionRaised e) ->
                    throw e

writeProcess :: (MonadSafe m, MonadIO (Base m), MonadIO m) => PipesProcess -> Consumer' ByteString m ()
writeProcess pipesProcess = loop `finally` (closeStdin pipesProcess) where
  loop = do 
        bs <- await
        lift $ liftIO $ atomically $ putTMVar (input pipesProcess) (Stdin bs)
        loop

closeStdin :: (MonadIO m) => PipesProcess -> m ()
closeStdin pipesProcess =
    liftIO $ atomically $ putTMVar (input pipesProcess) CloseStdin

flushProcess :: (MonadIO m) => PipesProcess -> m ()
flushProcess pipesProcess =
    liftIO $ atomically $ putTMVar (input pipesProcess) Flush