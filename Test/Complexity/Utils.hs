{-# LANGUAGE UnicodeSyntax #-}

{-|
Some utilities to quickly perform experiments.
-}

module Test.Complexity.Utils
    ( quickPerformExps
    , simpleMeasureNs
    , smartMeasure
    ) where

import Test.Complexity.Base   ( MeasurementStats
                              , Experiment
                              , InputSize
                              , performExperiment
                              , inputSizeFromList
                              , linearHeuristic
                              )
-- import Test.Complexity.Chart  ( showStatsChart )
import Test.Complexity.Pretty ( printStats )


quickPerformExps ∷ (α → IO MeasurementStats) → [α] → IO ()
quickPerformExps f xs = do stats ← mapM f xs
                           printStats     stats
                           -- showStatsChart stats
simpleMeasureNs ∷ [InputSize] → Integer → Double → [Experiment] → IO ()
simpleMeasureNs ns numSamples maxTime =
    quickPerformExps (performExperiment (inputSizeFromList ns) numSamples maxTime)

smartMeasure ∷ Double → InputSize → Integer → Double → [Experiment] → IO ()
smartMeasure step maxN numSamples maxTime xs =
    let tMax = maxTime / (fromIntegral $ length xs)
    in quickPerformExps (performExperiment (linearHeuristic step maxN) numSamples tMax) xs
