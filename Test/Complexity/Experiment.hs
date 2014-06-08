{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PackageImports            #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE UnicodeSyntax             #-}

module Test.Complexity.Experiment
    (
    -- *Experiments
      Description
    , Experiment
    , experiment
    , performExperiment
    ) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import "base" Control.Monad            ( liftM )
import "base" Data.Function            ( on )
import "base" Data.List                ( genericReplicate, sortBy )
import "base" System.Timeout           ( timeout )
import "base-unicode-symbols" Data.Function.Unicode ( (∘) )
import "base-unicode-symbols" Prelude.Unicode ( (⋅) )
import "criterion" Criterion ( Benchmarkable )
import "this" Test.Complexity.Sensors  ( Sensor )
import "this" Test.Complexity.Strategy ( Strategy(..) )
import "this" Test.Complexity.Types
import "deepseq" Control.DeepSeq       ( NFData, rnf )
import "time" Data.Time.Clock          ( getCurrentTime, diffUTCTime, UTCTime )
import "transformers" Control.Monad.IO.Class ( liftIO )
import qualified "vector" Data.Vector.Unboxed as V

-------------------------------------------------------------------------------
-- Experiments
-------------------------------------------------------------------------------

-- |A method of investigating the causal relationship between the size
--  of the input of an action and some aspect of the execution of the action.
data Experiment = ∀ α β. (NFData α, Benchmarkable β) ⇒
                  Experiment Description (Sensor α β) (InputGen α) (Action α β)

-- |Smart constructor for experiments.
experiment ∷ (NFData α, Benchmarkable β)
           ⇒ Description → (Sensor α β) → InputGen α → (Action α β) → Experiment
experiment = Experiment

-------------------------------------------------------------------------------

-- |Performs an experiment using a given strategy.
performExperiment ∷ Strategy
                  → Integer -- ^Number of samples per input size.
                  → Double  -- ^Maximum measure time in seconds (wall clock time).
                  → Experiment
                  → IO MeasurementStats
performExperiment (Strategy nextInput (run ∷ m [Sample] → IO [Sample]))
                  numSamples maxMeasureTime (Experiment desc sensor gen action)
    = liftM (MeasurementStats desc ∘ sortBy (compare `on` fst))
      ∘ run
      ∘ measureLoop 0 []
      =<< getCurrentTime
    where
      measureSample ∷ InputSize → IO Sample
      measureSample n = do xs ← measureAction gen action sensor numSamples n
                           case calcStats xs of
                             Just stats → return (n, stats)
                             Nothing → error "performExperiment: empty samples"

      measureLoop ∷ Int → [Sample] → UTCTime → m [Sample]
      measureLoop lenXs' xs' startTime = go lenXs' xs'
          where
            go ∷ Int → [Sample] → m [Sample]
            go lenXs xs = do
              curTime ← liftIO getCurrentTime
              let remTime = maxMeasureTime - realToFrac (diffUTCTime curTime startTime)
              if remTime > 0
                then nextInput lenXs xs remTime >>= \mN →
                     case mN of
                       Just n → liftIO ( timeout (round $ remTime ⋅ 1e6)
                                                 $ measureSample n
                                       )
                                 >>= maybe (return xs)
                                           (\x → go (lenXs + 1) $ x:xs)
                       _ → return xs
                else return xs

measureAction ∷ (NFData α)
              ⇒ InputGen α → Action α β → Sensor α β → Integer → InputSize → IO (V.Vector Double)
measureAction gen action sensor numSamples inputSize = fmap V.concat measure
  where
    measure ∷ IO [V.Vector Double]
    measure = let x = gen inputSize
              in rnf x `seq` mapM (sensor action) $ genericReplicate numSamples x
