module Main where

import Data.Maybe
import StreamRunner
import EventsHttpServer
import EventReaderStorage
import System.Environment

main :: IO ()
main = getArgs >>=
     \args -> case args of
            [] -> putStrLn "Invalid number of parameters to event stream runner.\n\
                  \Please supply the data generator location via a parameter in the command line."
            (dataGen:_) -> do 
                  readerStorage <- create
                  stream dataGen >>= maybe (return ()) (const $ httpService readerStorage)