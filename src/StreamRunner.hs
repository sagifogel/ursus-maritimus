{-# LANGUAGE QuasiQuotes #-}

module StreamRunner (stream)  where

import Text.Printf
import System.Directory

stream :: String -> IO (Maybe ())
stream path =
  doesFileExist path >>= \exists ->
    if exists then
      return $ Just ()
    else do 
      putStrLn $ printf "Data generator path %s is invalid.\n\ 
         \Please make sure the file exists at the specified location." path
      return Nothing
            