{-# LANGUAGE ExistentialQuantification #-}

module Test.Complexity.Debug where

import Control.Monad (replicateM)
import Control.Parallel.Strategies (NFData)
import Data.Function (fix)
import Data.List (genericIndex, genericTake, nub, group, sort, unfoldr)
import System.CPUTime (getCPUTime)

import qualified Data.List as L
import qualified Data.Map as M
import Control.Arrow (second)

import Test.Complexity
import qualified Test.Complexity.Pretty as PP
import qualified Test.Complexity.Chart  as Ch

data Test = forall a b. (NFData a, NFData b) => Test String (SizeGen a) (a -> b)

testCH :: [Test] -> Int -> [Integer] -> IO ()
testCH fs i ns = Ch.quickToChart =<< mapM (\(Test l g f) -> measureNs l g (strictAction f) i ns) fs

testPP :: [Test] -> Int -> [Integer] -> IO ()
testPP fs i ns = do PP.quickPrint =<< mapM (\(Test l g f) -> measureNs l g (strictAction f) i ns) fs

test :: [Test] -> Int -> [Integer] -> IO ()
test fs i ns = do mStats <- mapM (\(Test l g f) -> measureNs l g (strictAction f) i ns) fs
                  PP.quickPrint   mStats
                  Ch.quickToChart mStats

test' :: [Test] -> Int -> Double -> Double -> IO ()
test' fs i timeInc maxTime = let tMax = maxTime / (fromIntegral $ length fs)
                             in
    do mStats <- mapM (\(Test l g f) -> smartMeasure l g (strictAction f) i 10 4096 timeInc tMax) fs
       PP.quickPrint   mStats
       Ch.quickToChart mStats

-------------------------------------------------------------------------------

mkIntList :: Integer -> [Int]
mkIntList n = let n' = fromInteger n
              in [n', n' - 1 .. 0]

mkIntList2 :: Integer -> [Int]
mkIntList2 n = genericTake n $ go 0
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

bsort :: Ord a => [a] -> [a]
bsort [] = []
bsort xs = iterate swapPass xs !! (length xs - 1)
   where swapPass (x:y:zs) | x > y     = y : swapPass (x:zs)
                           | otherwise = x : swapPass (y:zs)
         swapPass xs = xs

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort (filter (< x) xs) ++ [x] ++ qsort (filter (>= x) xs)

cycle1, cycle2, cycle3 :: [a] -> [a]
cycle1 xs = xs ++ cycle1 xs
cycle2 xs = foldr (:) (cycle2 xs) xs
cycle3 xs = ys where ys = foldr (:) ys xs

zoei1, zoei2, zoei3, zoei4 :: Integer -> Int
zoei1 n = sum $ take (fromInteger n) $ cycle1 [1..1000]
zoei2 n = sum $ take (fromInteger n) $ cycle2 [1..1000]

zoei3 n = sum $ take (fromInteger n) $ cycle3 [1..1000]
zoei4 n = sum $ genericTake n $ cycle3 [1..1000]

bla1 :: Integer -> Int
bla1 n = last $ take (fromInteger n) $ cycle1 [1..1000]

bla2 :: Integer -> Int
bla2 n = last $ take (fromInteger n) $ cycle2 [1..1000]

bla3 :: Integer -> Int
bla3 n = last $ take (fromInteger n) $ cycle3 [1..1000]

testFibs :: [Test]
testFibs = [-- Test "fib1" (return . id) fib1
             Test "fib2" (return . id) fib2
           , Test "fib3" (return . id) fib3
           , Test "fib4" (return . id) fib4
           , Test "fib5" (return . id) fib5
           , Test "fib6" (return . id) fib6
           ]

-------------------------------------------------------------------------------

testFib1, testFib2, testNub, testGroup :: [Test]
testFib1   = [Test "fib1"   (return . id)         fib1]
testFib2   = [Test "fib2"   (return . id)         fib2]
testNub    = [Test "nub"    (return . mkIntList2) nub]
testGroup  = [Test "group"  (return . mkIntList2) group]

testBSort, testQSort, testSort :: [Test]
testBSort  = [Test "bubble" (return . mkIntList2) bsort]
testQSort  = [Test "qsort"  (return . mkIntList2) qsort]
testSort   = [Test "sort"   (return . mkIntList2) sort]


sizeL :: Integer -> (Int, [(Int, Int)])
sizeL n = let n' = fromInteger n
          in (n', [(m,m) | m <- [1..n']])

sizeM :: Integer -> (Int, M.Map Int Int)
sizeM = second (M.fromList) . sizeL

lookL (n, as) = L.lookup n as
lookM (n, as) = M.lookup n as


xs :: [Test]
xs = [ --Test "l" (return . sizeL) lookL
      Test "m" (return . sizeM) lookM
     ]
-------------------------------------------------------------------------------

zoei :: Int -> IO ()
zoei n = do ts <- replicateM n getCPUTime
            let ds = filter (/= 0) $ (zipWith (-) (tail ts) ts)
            mapM_ print $ map picoToMilli ds
