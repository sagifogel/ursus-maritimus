module Main where

import StreamRunner
import System.Process
import System.Environment

main :: IO ()
main = getArgs >>= stream