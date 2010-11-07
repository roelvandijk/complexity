{-# LANGUAGE UnicodeSyntax #-}

module Test.Complexity.Results 
    ( -- *Measurement results
      MeasurementStats(..)
    , Sample
    , Stats(..)
    ) where

-------------------------------------------------------------------------------
-- Measurement results
-------------------------------------------------------------------------------

-- TODO: declared somewhere else
type Description = String
type InputSize = Integer


-- |Statistics about a measurement performed on many inputs.
data MeasurementStats = MeasurementStats { msDesc    ∷ Description
                                         , msSamples ∷ [Sample]
                                         } deriving Show

-- |Statistics about the sampling of a single input value.
type Sample = (InputSize, Stats)

-- |Statistics about a collection of values.
data Stats = Stats { statsMin     ∷ Double -- ^Minimum value.
                   , statsMax     ∷ Double -- ^Maximum value.
                   , statsStdDev  ∷ Double -- ^Standard deviation
                   , statsMean    ∷ Double -- ^Arithmetic mean.
                   , statsMean2   ∷ Double
                   -- ^Mean of all samples that lie within one
                   --  standard deviation from the mean.
                   , statsSamples ∷ [Double] -- ^Samples from which these statistics are derived.
                   } deriving Show
