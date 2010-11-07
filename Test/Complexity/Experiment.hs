{-# LANGUAGE ExistentialQuantification    
           , ScopedTypeVariables         
           , UnicodeSyntax
  #-}

module Test.Complexity.Experiment
    ( 
    -- *Experiments
      Description
    , Experiment
    , experiment
    , pureExperiment
    , performExperiment
    ) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Control.Monad            ( liftM )
import Data.Function            ( on )
import Data.List                ( genericReplicate, sortBy )
import System.Timeout           ( timeout )

-- from base-unicode-symbols:
import Data.Function.Unicode    ( (∘) ) 

-- from complexity:
import Test.Complexity.Misc     ( diff, (>>=|) )
import Test.Complexity.Results  ( MeasurementStats(..)
                                , Sample
                                , Stats(..)
                                )
import Test.Complexity.Sensors  ( Sensor )
import Test.Complexity.Strategy ( Strategy(..) )
import Test.Complexity.Types    ( Action, InputGen, InputSize )

-- from deepseq:
import Control.DeepSeq          ( NFData )

-- from hstats:
import Math.Statistics          ( stddev, mean )

-- from time:
import Data.Time.Clock          ( getCurrentTime, diffUTCTime, UTCTime )

-- from transformers:
import Control.Monad.IO.Class   ( MonadIO, liftIO )


-------------------------------------------------------------------------------
-- Experiments
-------------------------------------------------------------------------------

-- |A description of an experiment.
type Description = String

-- |A method of investigating the causal relationship between the size
--  of the input of an action and some aspect of the execution of the action.
data Experiment = ∀ α β. NFData α ⇒
                  Experiment Description (Sensor α β) (InputGen (IO α)) (Action α β)

-- |Smart constructor for experiments.
experiment ∷ NFData α ⇒ Description → (Sensor α β) → InputGen (IO α) → (Action α β) → Experiment
experiment = Experiment

-- |Smart constructor for experiments on pure functions.
pureExperiment ∷ NFData α ⇒ Description → (Sensor α β) → InputGen α → (α → β) → Experiment
pureExperiment desc sensor gen f = experiment desc sensor (return ∘ gen) (return ∘ f)

-------------------------------------------------------------------------------

-- |Performs an experiment using a given strategy.
performExperiment ∷ Strategy [Sample]
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
      measureSample = measureAction gen action sensor numSamples

      measureLoop ∷ Int → [Sample] → UTCTime → m [Sample]
      measureLoop lenXs' xs' startTime = go lenXs' xs'
          where
            go lenXs xs = do
              curTime ← liftIO getCurrentTime
              let remTime = maxMeasureTime - realToFrac (diffUTCTime curTime startTime)
              if remTime > 0
                then nextInput lenXs xs remTime >>= \mN →
                     case mN of
                       Just n → liftIO ( timeout (round $ remTime * 1e6)
                                                  $ measureSample n
                                        )
                                 >>= maybe (return xs)
                                           (\x → go (lenXs + 1) $ x:xs)
                       _      → return xs
                else return xs

-- | Measure the time needed to evaluate an action when applied to an
-- input of size \'n\'.
measureAction ∷ NFData α
              ⇒ InputGen (IO α) → Action α β → Sensor α β → Integer → InputSize → IO Sample
measureAction gen action sensor numSamples inputSize = fmap (\ys → (inputSize, calculateStats ys))
                                                            measure
    where
      measure ∷ IO [Double]
      measure = gen inputSize >>=| \x → mapM (sensor action) $ genericReplicate numSamples x


-------------------------------------------------------------------------------
-- Misc
-------------------------------------------------------------------------------

-- |Calculate statistics about a collection of values.
--
-- Precondition: not $ null xs
calculateStats ∷ [Double] → Stats
calculateStats xs = Stats { statsMin     = minimum xs
                          , statsMax     = maximum xs
                          , statsStdDev  = stddev_xs
                          , statsMean    = mean_xs
                          , statsMean2   = mean2_xs
                          , statsSamples = xs
                          }
    where stddev_xs = stddev xs
          mean_xs   = mean xs
          mean2_xs | null inStddev = mean_xs
                   | otherwise     = mean inStddev
          inStddev = filter (\x → diff mean_xs x < stddev_xs) xs
