module Main where

import Test.Complexity
import Test.Complexity.Debug

main :: IO ()
main = test testFibs 5 [0,1000..30000]
