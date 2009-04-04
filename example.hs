module Main where

import Data.Function (fix)
import Data.List (sort, unfoldr)
import System.Environment (getArgs)
import qualified Data.List   as L
import qualified Data.Map    as M
import qualified Data.IntMap as IM

import System.Random.Mersenne

import Test.Complexity
import Test.Complexity.Pretty (printStats)
import Test.Complexity.Chart  (showStatsChart)

-------------------------------------------------------------------------------

quickMeasure :: (Measurable -> IO MeasurementStats) -> [Measurable] -> IO ()
quickMeasure f xs =
    do stats <- mapM f xs
       printStats     stats
       showStatsChart stats

simpleMeasureNs :: [InputSize] -> Int -> Double -> Double -> [Measurable] -> IO ()
simpleMeasureNs ns numSamples minSampleTime maxTime =
    quickMeasure (measureNs ns numSamples minSampleTime maxTime)

simpleSmartMeasure :: Double -> InputSize -> Int -> Double -> Double -> [Measurable] -> IO ()
simpleSmartMeasure step maxN numSamples minSampleTime maxTime xs =
    let tMax = maxTime / (fromIntegral $ length xs)
    in quickMeasure (smartMeasure step maxN numSamples minSampleTime tMax) xs

-------------------------------------------------------------------------------

mkIntList :: InputSize -> [Int]
mkIntList n = let n' = fromInteger n
              in [n', n' - 1 .. 0]

mkIntList2 :: InputSize -> [Int]
mkIntList2 n = take (fromInteger n) $ go 0
    where go x = x : go (1 + x `mod` 7)

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

mkMap :: InputSize -> (Int, M.Map Int Int)
mkMap n = let n' = fromInteger n
          in (n' `div` 2, M.fromList [(k, k) | k <- [0 .. n']])

mkMap2 :: InputSize -> IO (Int, M.Map Int Int)
mkMap2 n = let n' = fromInteger n
           in do f <- random =<< getStdGen :: IO Double
                 let x = floor $ f * (fromIntegral n)
                 return (x, M.fromList [(k, k) | k <- [0 .. n']])

mkIntMap :: InputSize -> (Int, IM.IntMap Int)
mkIntMap n = let n' = fromInteger n
             in (n' `div` 2, IM.fromList [(k, k) | k <- [0 .. n']])

benchMaps :: [Measurable]
benchMaps = [ measurable "Data.Map pure"   (return . mkMap)    (return . uncurry M.lookup)
            , measurable "Data.IntMap" (return . mkIntMap) (return . uncurry IM.lookup)
            ]

-------------------------------------------------------------------------------

cmdLine :: [Measurable] -> IO ()
cmdLine xs = do args <- getArgs
                if length args == 2
                  then let (a1:a2:_) = take 2 args
                           maxTime   = (read a1) :: Double
                           maxN      = (read a2) :: InputSize
                       in simpleSmartMeasure 1.1 maxN 10 10 maxTime xs
--                     in simpleMeasureNs [1..20] 10 10 120 xs
                  else putStrLn "Error: I need 2 arguments (max time and max input size)"

main :: IO ()
main = cmdLine benchSorts
