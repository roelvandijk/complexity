{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Main where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "array" Data.Array.IArray  ( assocs )
import "array" Data.Array.MArray  ( newArray, readArray, writeArray )
import "array" Data.Array.ST      ( runSTUArray )
import "array" Data.Array.Unboxed ( UArray, accumArray, listArray )
import qualified "array" Data.Array.IArray as IA
import "base" Control.Monad      ( forM_ )
import "base" Data.Function      ( fix )
import "base" Data.List          ( sort, unfoldr, genericReplicate )
import "base" Data.Word          ( Word8 )
import qualified "base" Data.Bits as B
import "base-unicode-symbols" Data.Eq.Unicode       ( (≡) )
import "base-unicode-symbols" Data.Function.Unicode ( (∘) )
import "base-unicode-symbols" Prelude.Unicode       ( (⋅) )
import "deepseq" Control.DeepSeq ( NFData(..) )
import qualified "containers" Data.Map    as M
import qualified "containers" Data.IntMap as IM
import "this" Test.Complexity
import "transformers" Control.Monad.IO.Class ( liftIO )
import "criterion" Criterion ( nf )
import "criterion" Criterion.Config ( Config, defaultConfig, cfgSamples, ljust )
import "criterion" Criterion.Environment ( Environment, measureEnvironment )
import "criterion" Criterion.Monad ( withConfig, getConfig )

-------------------------------------------------------------------------------
-- Some input generators for lists of Int

genIntList ∷ InputGen [Int]
genIntList n = let n' = fromInteger n
               in [[n', n' - 1 .. 0]]

-- Very simple pseudo random number generator.
pseudoRnd ∷ Integral n ⇒ n → n → n → n → [n]
pseudoRnd p1 p2 n d = iterate (\x → (p1 ⋅ x + p2) `mod` n) d

genIntList2 ∷ InputGen [Int]
genIntList2 n = chunk (fromInteger n) $ pseudoRnd 16807 0 (2 ^ (31 ∷ Int) - 1) 79

chunk ∷ Int → [α] → [[α]]
chunk n xs = let (as, bs) = splitAt n xs
             in as : chunk n bs

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

{-
expFibs ∷ [Experiment]
expFibs = --[ experiment "fib0" (cpuTimeSensor 10) id fib0
          --, experiment "fib1" (cpuTimeSensor 10) id fib1
          --] ++
          [ experiment "fib2" (cpuTimeSensor 10) id fib2
          , experiment "fib3" (cpuTimeSensor 10) id fib3
          , experiment "fib4" (cpuTimeSensor 10) id fib4
          , experiment "fib5" (cpuTimeSensor 10) id fib5
          , experiment "fib6" (cpuTimeSensor 10) id fib6
          ]
-}

-------------------------------------------------------------------------------
-- Sorting algorithms

bsort ∷ Ord a ⇒ [a] → [a]
bsort [] = []
bsort xs = iterate swapPass xs !! (length xs - 1)
   where swapPass (x:y:zs) | x > y     = y : swapPass (x:zs)
                           | otherwise = x : swapPass (y:zs)
         swapPass zs = zs


qsort ∷ Ord a ⇒ [a] → [a]
qsort []     = []
qsort (x:xs) = qsort (filter (< x) xs) ++ [x] ++ qsort (filter (>= x) xs)

-------------------------------------------------------------------------------
-- Map lookups

mkMap ∷ InputSize → (Int, M.Map Int Int)
mkMap n = let n' = fromInteger n
          in (n' `div` 2, M.fromList [(k, k) | k ← [0 .. n']])

mkIntMap ∷ InputSize → (Int, IM.IntMap Int)
mkIntMap n = let n' = fromInteger n
             in (n' `div` 2, IM.fromList [(k, k) | k ← [0 .. n']])

{-
expMaps ∷ [Experiment]
expMaps = [ experiment "Data.Map pure" (cpuTimeSensor 10) mkMap    (uncurry M.lookup)
          , experiment "Data.IntMap"   (cpuTimeSensor 10) mkIntMap (uncurry IM.lookup)
          ]
-}

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

{-
expHistogram ∷ [Experiment]
expHistogram = [ experiment "M.fromListWith" (cpuTimeSensor 10) mkByteList histogram1
               , experiment "runSTUArray"    (cpuTimeSensor 10) mkByteList histogram2
               , experiment "accumArray"     (cpuTimeSensor 10) mkByteList histogram3
               ]
-}

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
            toPolynomial_b i = let (q, r) = i `qrf` b
                               in r : toPolynomial_b q

            qrf | b > 0     = quotRem
                | otherwise = quotRem'

            quotRem' ∷ Integral a ⇒ a → a → (a, a)
            quotRem' i d = let qr@(q, r) = i `quotRem` d
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
countBits1 x = go x
    where go 0 = 0
          go n = (if odd n then 1 else 0) + go (B.shiftR n 1)

countBits2 ∷ Integer → Integer
countBits2 0 = 0
countBits2 x = let foo = (logBase 2 $ fromIntegral x) ∷ Double
               in 1 + (countBits2 $ x - (2 ^ (floor foo ∷ Int)))

{-
expBits ∷ [Experiment]
expBits = [ experiment "count bits map 4"    (cpuTimeSensor 10) id (mkCountBits_map   4)
          , experiment "count bits map 8"    (cpuTimeSensor 10) id (mkCountBits_map   8)
          , experiment "count bits array 4"  (cpuTimeSensor 10) id (mkCountBits_array 4)
          , experiment "count bits array 8"  (cpuTimeSensor 10) id (mkCountBits_array 8)
          ]
-}

-------------------------------------------------------------------------------



expBSort ∷ Environment → Config → Experiment
expBSort env conf = experiment "bubble sort"
                               (criterionSensor env conf)
                               genIntList2
                               (nf bsort)

expQSort ∷ Environment → Config → Experiment
expQSort env conf = experiment "quick sort"
                               (criterionSensor env conf)
                               genIntList2
                               (nf qsort)

expSort ∷ Environment → Config → Experiment
expSort env conf = experiment "Data.List.sort"
                              (criterionSensor env conf)
                              genIntList2
                              (nf sort)

main ∷ IO ()
main = withConfig (defaultConfig {cfgSamples = ljust 10}) $ do
         env ← measureEnvironment
         conf ← getConfig
         liftIO $ defaultMain $ map (\e → e env conf) [expQSort, expSort]

