module Main where

import System.Environment (getArgs)

import Test.Complexity
import Test.Complexity.Debug

main :: IO ()
main = test' (testQSort ++ testSort) 10 1.1 . read . head =<< getArgs
