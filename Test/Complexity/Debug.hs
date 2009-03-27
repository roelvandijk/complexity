{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BangPatterns #-}

module Test.Complexity.Debug where

import Control.Monad (replicateM, replicateM_)
import Control.Parallel            (pseq)
import Control.Parallel.Strategies (NFData)
import Data.Function (fix)
import Data.List (genericIndex, genericTake, nub, group, sort, unfoldr, foldl')
import System.CPUTime (getCPUTime)
import Math.Statistics (mean)
import qualified Data.List as L
import qualified Data.Map as M
import Control.Arrow (second)

import Test.Complexity
import qualified Test.Complexity.Pretty as PP
import qualified Test.Complexity.Chart  as Ch

data Test = forall a b. (NFData a, NFData b) => Test String (SizeGen a) (Action a b)

testCH :: [Test] -> Int -> [Integer] -> IO ()
testCH fs i ns = Ch.quickToChart =<< mapM (\(Test l g f) -> measureNs l g f i ns) fs

testPP :: [Test] -> Int -> [Integer] -> IO ()
testPP fs i ns = do PP.quickPrint =<< mapM (\(Test l g f) -> measureNs l g f i ns) fs

test :: [Test] -> Int -> [Integer] -> IO ()
test fs i ns = do mStats <- mapM (\(Test l g f) -> measureNs l g f i ns) fs
                  PP.quickPrint   mStats
                  Ch.quickToChart mStats

test' :: [Test] -> Int -> Double -> Double -> Integer -> IO ()
test' fs i timeInc maxTime maxN = let tMax = maxTime / (fromIntegral $ length fs)
                                  in
    do mStats <- mapM (\(Test l g f) -> smartMeasure l g f i 10 timeInc tMax maxN) fs
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

testFibs :: [Test]
testFibs = [ Test "fib1" (return . id) (return . fib1)
           , Test "fib2" (return . id) (return . fib2)
           , Test "fib3" (return . id) (return . fib3)
           , Test "fib4" (return . id) (return . fib4)
           , Test "fib5" (return . id) (return . fib5)
           , Test "fib6" (return . id) (return . fib6)
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

testBSort, testQSort, testSort :: [Test]
testBSort  = [Test "bubble sort"    (return . mkIntList2) (return . bsort)]
testQSort  = [Test "quick sort"     (return . mkIntList2) (return . qsort)]
testSort   = [Test "Data.List.sort" (return . mkIntList2) (return . sort)]
testSorts  = testBSort ++ testQSort ++ testSort
