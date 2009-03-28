module Main where

import Data.Function (fix)
import Data.List (sort, unfoldr)
import System.Environment (getArgs)
import qualified Data.List as L

import Test.Complexity
import qualified Test.Complexity.Pretty as PP
import qualified Test.Complexity.Chart  as Ch

-------------------------------------------------------------------------------

benchCH :: [Measurable] -> Int -> [Integer] -> IO ()
benchCH bs i ns = Ch.quickToChart =<< mapM (measureNs i ns) bs

benchPP :: [Measurable] -> Int -> [Integer] -> IO ()
benchPP bs i ns = PP.quickPrint =<< mapM (measureNs i ns) bs

bench :: [Measurable] -> Int -> [Integer] -> IO ()
bench bs i ns = do mStats <- mapM (measureNs i ns) bs
                   PP.quickPrint   mStats
                   Ch.quickToChart mStats

bench' :: [Measurable] -> Int -> Double -> Double -> Integer -> IO ()
bench' bs i timeInc maxTime maxN = let tMax = maxTime / (fromIntegral $ length bs)
                                   in
    do mStats <- mapM (smartMeasure i 10 timeInc tMax maxN) bs
       PP.quickPrint   mStats
       Ch.quickToChart mStats

-------------------------------------------------------------------------------

mkIntList :: Integer -> [Int]
mkIntList n = let n' = fromInteger n
              in [n', n' - 1 .. 0]

mkIntList2 :: Integer -> [Int]
mkIntList2 n = take (fromInteger n) $ go 0
    where go x = x : go (1 + x `mod` 7)

mkIntList3 :: Integer -> [Int]
mkIntList3 n = take (fromInteger n) $ go 0
    where go x = x : go (1 + x `mod` 31)

-------------------------------------------------------------------------------

fib0 :: Integer -> Integer
fib0 0 = 0
fib0 1 = 1
fib0 n = fib0 (n - 1) + fib0 (n - 2)

fib1 :: Integer -> Integer
fib1 0 = 0
fib1 1 = 1
fib1 n | even n         = f1 * (f1 + 2 * f2)
       | n `mod` 4 == 1 = (2 * f1 + f2) * (2 * f1 - f2) + 2
       | otherwise      = (2 * f1 + f2) * (2 * f1 - f2) - 2
   where k = n `div` 2
         f1 = fib1 k
         f2 = fib1 (k-1)

fib2 :: Integer -> Integer
fib2 n = fibs !! fromInteger n
    where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fib3 :: Integer -> Integer
fib3 n = fibs !! fromInteger n
    where fibs = scanl (+) 0 (1:fibs)

fib4 :: Integer -> Integer
fib4 n = fibs !! fromInteger n
    where fibs = fix (scanl (+) 0 . (1:))

fib5 :: Integer -> Integer
fib5 n = fibs !! fromInteger n
    where fibs = unfoldr (\(a,b) -> Just (a,(b, a+b))) (0,1)

fib6 :: Integer -> Integer
fib6 n = fibs !! fromInteger n
    where fibs = map fst $ iterate (\(a,b) -> (b, a+b)) (0,1)

benchFibs :: [Measurable]
benchFibs = [ pureMeasurable "fib1" id fib1
            , pureMeasurable "fib2" id fib2
            , pureMeasurable "fib3" id fib3
            , pureMeasurable "fib4" id fib4
            , pureMeasurable "fib5" id fib5
            , pureMeasurable "fib6" id fib6
            ]

-------------------------------------------------------------------------------

bsort :: Ord a => [a] -> [a]
bsort [] = []
bsort xs = iterate swapPass xs !! (length xs - 1)
   where swapPass (x:y:zs) | x > y     = y : swapPass (x:zs)
                           | otherwise = x : swapPass (y:zs)
         swapPass xs = xs

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort (filter (< x) xs) ++ [x] ++ qsort (filter (>= x) xs)

benchBSort, benchQSort, benchSort, benchSorts :: [Measurable]
benchBSort  = [pureMeasurable "bubble sort"    mkIntList2 bsort]
benchQSort  = [pureMeasurable "quick sort"     mkIntList2 qsort]
benchSort   = [pureMeasurable "Data.List.sort" mkIntList2 sort]
benchSorts  = benchBSort ++ benchQSort ++ benchSort

-------------------------------------------------------------------------------

main :: IO ()
main = do args <- getArgs
          if length args == 2
            then let (a1:a2:_) = take 2 args
                     maxTime   = (read a1) :: Double
                     maxN      = (read a2) :: Integer
                 in bench' stuff 5 1.1 maxTime maxN
            else putStrLn "Error: I need 2 arguments (max time and max input size)"
    where stuff = benchQSort


