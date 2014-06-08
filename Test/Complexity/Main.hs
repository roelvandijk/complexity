{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax  #-}

module Test.Complexity.Main where

import "base" Control.Monad ( forM_ )
import "base" Text.Printf ( printf )
import "this" Test.Complexity.Experiment ( Experiment, performExperiment )
import "this" Test.Complexity.Strategy   ( limitSamples, linearHeuristic )
import "this" Test.Complexity.Types
import "this" Test.Complexity.Chart ( showStatsChart )
import qualified "vector" Data.Vector.Unboxed as V ( length )

defaultMain ∷ [Experiment] → IO ()
defaultMain experiments = do
    stats ← mapM f experiments
    mapM_ printStats stats
    showStatsChart stats
  where
    f experiment = performExperiment strategy 1 10 60.0 experiment
    strategy = limitSamples 3 $ linearHeuristic 1.2 10000


printStats ∷ MeasurementStats → IO ()
printStats ms = do
    putStrLn $ msDesc ms
    printf "n, min, mean, max, #\n"
    forM_ (msSamples ms) $ \(n, Stats{..}) →
      printf "%3i, %.9f, %.9f, %.9f, %3i\n" n stMin stMean stMax (V.length stSamples)
    putStrLn ""

