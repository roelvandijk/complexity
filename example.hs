{-# LANGUAGE FlexibleInstances 
           , RankNTypes
           , UnicodeSyntax
  #-}

module Main where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Monad         ( forM_ )
import Data.Array.IArray     ( assocs )
import Data.Array.MArray     ( newArray, readArray, writeArray )
import Data.Array.ST         ( runSTUArray )
import Data.Array.Unboxed    ( UArray, accumArray, listArray )
import Data.Function         ( fix )
import Data.List             ( sort, unfoldr, genericReplicate )
import Data.Word             ( Word8 )
import System.Environment    ( getArgs )

import qualified Data.Array.IArray as IA
import qualified Data.Bits   as B

-- from base-unicode-symbols:
import Data.Eq.Unicode       ( (≡) )
import Data.Function.Unicode ( (∘) ) 
import Prelude.Unicode       ( (⋅) )

-- from deepseq:
import Control.DeepSeq       ( NFData(..) )

-- from containers:
import qualified Data.Map    as M
import qualified Data.IntMap as IM

-- from complexity:
import Test.Complexity
import Test.Complexity.Main


-------------------------------------------------------------------------------
-- Some input generators for lists of Int

genIntList ∷ InputGen [Int]
genIntList n = let n' = fromInteger n
              in [n', n' - 1 .. 0]

-- Very simple pseudo random number generator.
pseudoRnd ∷ Integral n ⇒ n → n → n → n → [n]
pseudoRnd p1 p2 n d = iterate (\x → (p1 ⋅ x + p2) `mod` n) d

genIntList2 ∷ InputGen [Int]
genIntList2 n = take (fromInteger n) $ pseudoRnd 16807 0 (2 ^ (31 ∷ Int) - 1) 79

-------------------------------------------------------------------------------
-- Bunch of fibonacci functions

fib0 ∷ Integer → Integer
fib0 0 = 0
fib0 1 = 1
fib0 n = fib0 (n - 1) + fib0 (n - 2)

fib1 ∷ Integer → Integer
fib1 0 = 0
fib1 1 = 1
fib1 n | even n        = f1 ⋅ (f1 + 2 ⋅ f2)
       | n `mod` 4 ≡ 1 = (2 ⋅ f1 + f2) ⋅ (2 ⋅ f1 - f2) + 2
       | otherwise     = (2 ⋅ f1 + f2) ⋅ (2 ⋅ f1 - f2) - 2
   where k = n `div` 2
         f1 = fib1 k
         f2 = fib1 (k-1)

fib2 ∷ Integer → Integer
fib2 n = fibs !! fromInteger n
    where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fib3 ∷ Integer → Integer
fib3 n = fibs !! fromInteger n
    where fibs = scanl (+) 0 (1:fibs)

fib4 ∷ Integer → Integer
fib4 n = fibs !! fromInteger n
    where fibs = fix (scanl (+) 0 ∘ (1:))

fib5 ∷ Integer → Integer
fib5 n = fibs !! fromInteger n
    where fibs = unfoldr (\(a, b) → Just (a, (b, a + b))) (0,1)

fib6 ∷ Integer → Integer
fib6 n = fibs !! fromInteger n
    where fibs = map fst $ iterate (\(a, b) → (b, a + b)) (0,1)

expFibs ∷ [Experiment]
expFibs = --[ pureExperiment "fib0" (cpuTimeSensor 10) id fib0
          --, pureExperiment "fib1" (cpuTimeSensor 10) id fib1
          --] ++
          [ pureExperiment "fib2" (cpuTimeSensor 10) id fib2
          , pureExperiment "fib3" (cpuTimeSensor 10) id fib3
          , pureExperiment "fib4" (cpuTimeSensor 10) id fib4
          , pureExperiment "fib5" (cpuTimeSensor 10) id fib5
          , pureExperiment "fib6" (cpuTimeSensor 10) id fib6
          ]

-------------------------------------------------------------------------------
-- Sorting algorithms

bsort ∷ Ord a ⇒ [a] → [a]
bsort [] = []
bsort xs = iterate swapPass xs !! (length xs - 1)
   where swapPass (x:y:zs) | x > y     = y : swapPass (x:zs)
                           | otherwise = x : swapPass (y:zs)
         swapPass xs = xs

qsort ∷ Ord a ⇒ [a] → [a]
qsort []     = []
qsort (x:xs) = qsort (filter (< x) xs) ++ [x] ++ qsort (filter (>= x) xs)

expBSort, expQSort, expSort, expSorts ∷ [Experiment]
expBSort  = [pureExperiment "bubble sort"    (cpuTimeSensor 10) genIntList2 bsort]
expQSort  = [pureExperiment "quick sort"     (cpuTimeSensor 10) genIntList2 qsort]
expSort   = [pureExperiment "Data.List.sort" (cpuTimeSensor 10) genIntList2 sort]
expSorts  = expBSort ++ expQSort ++ expSort

-------------------------------------------------------------------------------
-- Map lookups

mkMap ∷ InputSize → (Int, M.Map Int Int)
mkMap n = let n' = fromInteger n
          in (n' `div` 2, M.fromList [(k, k) | k ← [0 .. n']])

mkIntMap ∷ InputSize → (Int, IM.IntMap Int)
mkIntMap n = let n' = fromInteger n
             in (n' `div` 2, IM.fromList [(k, k) | k ← [0 .. n']])

expMaps ∷ [Experiment]
expMaps = [ pureExperiment "Data.Map pure" (cpuTimeSensor 10) mkMap    (uncurry M.lookup)
          , pureExperiment "Data.IntMap"   (cpuTimeSensor 10) mkIntMap (uncurry IM.lookup)
          ]

-------------------------------------------------------------------------------
-- Histogram

instance NFData (UArray Word8 Int) where
    rnf = rnf ∘ assocs

mkByteList ∷ InputSize → [Word8]
mkByteList n = take (fromInteger n) $ map toByte xs
    where xs ∷ [Int]
          xs = pseudoRnd 16807 0 (2 ^ (31 - 1 ∷ Int)) 79
          toByte x = ceiling ((255 ⋅ (fromIntegral x) / (2 ^ (31 - 1 ∷ Int))) ∷ Double)

histogram1 ∷ (Ord a) ⇒ [a] → M.Map a Integer
histogram1 = M.fromListWith (+) ∘ map (\k → (k, 1))

histogram2 ∷ [Word8] → UArray Word8 Int
histogram2 input = runSTUArray $ do
  array ← newArray (0, 255) 0
  forM_ input $ \i → do
    x ← readArray array i
    writeArray array i (x + 1)
  return array

histogram3 ∷ [Word8] → UArray Word8 Int
histogram3 = accumArray (+) 0 (0, 255) ∘ map (\k → (k, 1))

expHistogram ∷ [Experiment]
expHistogram = [ pureExperiment "M.fromListWith" (cpuTimeSensor 10) mkByteList histogram1
               , pureExperiment "runSTUArray"    (cpuTimeSensor 10) mkByteList histogram2
               , pureExperiment "accumArray"     (cpuTimeSensor 10) mkByteList histogram3
               ]

-------------------------------------------------------------------------------
-- Bit counting

toPolynomial ∷ Integer → Integer → [Integer]
toPolynomial b n | n ≡ 0    = [0]
                 | b ≡ 0    = error "toPolynomial: base 0"
                 | b ≡ (-1) = case n of
                                 (-1) → [0, 1]
                                 1    → [1]
                                 _    → error "toPolynomial: base (-1)"
                 | b ≡ 1    = genericReplicate (abs n) (signum n)
                 | otherwise = toPolynomial_b $ n
      where toPolynomial_b 0 = []
            toPolynomial_b n = let (q, r) = n `qr` b
                         in r : toPolynomial_b q

            qr | b > 0     = quotRem
               | otherwise = quotRem'

            quotRem' ∷ Integral a ⇒ a → a → (a, a)
            quotRem' n d = let qr@(q, r) = n `quotRem` d
                           in if r < 0
                              then (q + 1, r - d)
                              else qr

mkBitMap ∷ Integer → IM.IntMap Int
mkBitMap b = IM.fromList $ [(fromInteger n, fromInteger $ countBits1 n) | n ← [0 .. b - 1]]

mkBitArray ∷ Integer → UArray Int Int
mkBitArray b = let b' = fromInteger b
               in listArray (0, b' - 1) $ map (fromInteger ∘ countBits1) [0 .. b - 1]

mkCountBits_map ∷ Integer → Integer → Integer
mkCountBits_map b = let r      = 2 ^ b
                        bitMap = mkBitMap r
                    in toInteger ∘ sum ∘ map ((bitMap IM.!) ∘ fromInteger) ∘ toPolynomial r

mkCountBits_array ∷ Integer → Integer → Integer
mkCountBits_array b =  let r = 2 ^ b
                           bitArr = mkBitArray r
                       in toInteger ∘ sum ∘ map ((bitArr IA.!) ∘ fromInteger) ∘ toPolynomial r

countBits1 ∷ Integer → Integer
countBits1 n = go n
    where go 0 = 0
          go n = (if odd n then 1 else 0) + go (B.shiftR n 1)

countBits2 ∷ Integer → Integer
countBits2 0 = 0
countBits2 x = let foo = (logBase 2 $ fromIntegral x) ∷ Double
               in 1 + (countBits2 $ x - (2 ^ (floor foo ∷ Int)))

expBits ∷ [Experiment]
expBits = [ pureExperiment "count bits map 4"    (cpuTimeSensor 10) id (mkCountBits_map   4)
          , pureExperiment "count bits map 8"    (cpuTimeSensor 10) id (mkCountBits_map   8)
          , pureExperiment "count bits array 4"  (cpuTimeSensor 10) id (mkCountBits_array 4)
          , pureExperiment "count bits array 8"  (cpuTimeSensor 10) id (mkCountBits_array 8)
          ]

-------------------------------------------------------------------------------


measure ∷ Double → InputSize → Integer → Double → [Experiment] → IO ()
measure step maxN numSamples maxTime xs =
    let tMax = maxTime / (fromIntegral $ length xs)
    in quickPerformExps (performExperiment (linearHeuristic step maxN) numSamples tMax) xs

cmdLine ∷ [Experiment] → IO ()
cmdLine xs = do args ← getArgs
                if length args ≡ 3
                  then let (a1:a2:a3:_) = take 3 args
                           step      = (read a1) ∷ Double
                           maxTime   = (read a2) ∷ Double
                           maxN      = (read a3) ∷ InputSize
                       in measure step maxN 10 maxTime xs
                  else putStrLn "Usage: <step> <maxTime> <maxN>"


main ∷ IO ()
-- main = defaultMain expSorts
main = cmdLine expSorts
