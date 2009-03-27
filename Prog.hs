module Main where

import System.Environment (getArgs)

import Test.Complexity
import Test.Complexity.Debug

main :: IO ()
main = do args <- getArgs
          if length args == 2
            then let (a1:a2:_) = take 2 args
                     maxTime   = (read a1) :: Double
                     maxN      = (read a2) :: Integer
                 in test' stuff 5 1.1 maxTime maxN
            else putStrLn "Error: I need 2 arguments (max time and max input size)"
    where stuff = testSorts


