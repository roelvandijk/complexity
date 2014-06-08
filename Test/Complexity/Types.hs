{-# LANGUAGE PackageImports #-}
{-# LANGUAGE UnicodeSyntax  #-}

module Test.Complexity.Types where

import "statistics" Statistics.Sample ( mean )
import qualified "vector" Data.Vector.Unboxed as V ( Vector, minimum, maximum, null )


-- |An Action is a function of which aspects of its execution can be measured.
type Action α β = α → β

-- |A input generator produces a value of a certain size.
type InputGen α = InputSize → α

-- |The size of an input on which an action is applied.
type InputSize = Integer

type Description = String

-- |Statistics about a measurement performed on many inputs.
data MeasurementStats = MeasurementStats { msDesc    ∷ Description
                                         , msSamples ∷ [Sample]
                                         } deriving Show

-- |Statistics about the sampling of a single input value.
type Sample = (InputSize, Stats)

data Stats = Stats { stMin     ∷ Double
                   , stMax     ∷ Double
                   , stMean    ∷ Double
                   , stSamples ∷ V.Vector Double
                   } deriving Show

calcStats ∷ V.Vector Double → Maybe Stats
calcStats xs | V.null xs = Nothing
             | otherwise = Just $ Stats { stMin     = V.minimum xs
                                        , stMax     = V.maximum xs
                                        , stMean    = mean xs
                                        , stSamples = xs
                                        }


