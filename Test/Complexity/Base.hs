{-# LANGUAGE ExistentialQuantification    
           , LiberalTypeSynonyms          
           , RankNTypes                   
           , ScopedTypeVariables         
           , UnicodeSyntax
  #-}

module Test.Complexity.Base
    ( -- *Measurement subject
      Action
    , InputGen
    , InputSize

    -- *Experiments
    , Description
    , Experiment
    , experiment
    , pureExperiment
    , performExperiment

    -- *Measurement strategy
    , Strategy(..)
    , inputSizeFromList
    , linearHeuristic
    , limitSamples

    -- *Sensors
    , Sensor
    , timeSensor
    , cpuTimeSensor
    , wallClockTimeSensor

    -- *Measurement results
    , MeasurementStats(..)
    , Sample
    , Stats(..)
    ) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Control.Arrow                  ( (***) )
import Control.Monad                  ( liftM )
import Data.Function                  ( on )
import Data.List                      ( genericReplicate, sortBy )
import System.CPUTime                 ( getCPUTime )
import System.Timeout                 ( timeout )

-- from base-unicode-symbols:
import Data.Ord.Unicode               ( (≤), (≥) )
import Data.Function.Unicode          ( (∘) ) 
import Prelude.Unicode                ( (⋅) )

-- from complexity:
import Test.Complexity.Fit            ( fitLinear, linearInv )
import Test.Complexity.Misc           ( strictReplicateM_
                                      , diff
                                      , picoToMilli
                                      , check
                                      , (>>=|)
                                      )

-- from deepseq:
import Control.DeepSeq                ( NFData )

-- from hstats:
import Math.Statistics                ( stddev, mean )

-- from time:
import Data.Time.Clock                ( getCurrentTime, diffUTCTime, UTCTime )

-- from transformers:
import Control.Monad.IO.Class         ( MonadIO, liftIO )
import Control.Monad.Trans.State.Lazy ( StateT, evalStateT, get, put )


-------------------------------------------------------------------------------
-- Measurement subject
-------------------------------------------------------------------------------

-- |An Action is a function of which aspects of its execution can be measured.
type Action α β = α → IO β

-- |A input generator produces a value of a certain size.
type InputGen α = InputSize → α

-- |The size of an input on which an action is applied.
type InputSize = Integer

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
-- Measurement strategy
-------------------------------------------------------------------------------

-- | A measurement 'Strategy' describes how an 'Experiment' should be
-- executed.
--
-- Its main responsibility is to provide the next 'InputSize' which
-- should be measured based on the data that is already gathered and
-- the remaining time. This is the role of the 'nextInputSize'
-- function. It lives in an arbitrary 'MonadIO' and you have to
-- provide a function which transforms this monad to an 'IO'
-- action. If a value of Nothing is produced this means that it can't
-- generate any more input sizes and measuring will stop.
data Strategy α = ∀ m. MonadIO m
                ⇒ Strategy
                   { -- | Function which calculates the next
                     -- 'InputSize' to measure. Arguments are, in
                     -- order: the number of samples measured thus
                     -- far, a list of previously measured samples and
                     -- the remaining time in seconds.
                     nextInputSize ∷ Int → [Sample] → Double → m (Maybe InputSize)
                     -- | Run function which lifts the strategy monad
                     -- to IO.
                   , runStrategy ∷ (m α → IO α)
                   }

-- | A strategy which produces input sizes from a given list.
--
-- When the list is consumed it will produce 'Nothing'.
inputSizeFromList ∷ [InputSize] → Strategy a
inputSizeFromList ns = Strategy (\_ _ _ → m) (\s → evalStateT s ns)
    where
      m ∷ StateT [InputSize] IO (Maybe InputSize)
      m = do xs ← get
             case xs of
               []      → return Nothing
               (x:xs') → do put xs'
                            return $ Just x

-------------------------------------------------------------------------------

-- | Transforms a strategy into one that can only see the last 'n'
-- samples.
limitSamples ∷ Int → Strategy a → Strategy a
limitSamples maxSamples (Strategy next run) =
    Strategy { nextInputSize = \n xs t → next n (take maxSamples xs) t
             , runStrategy   = run
             }

linearHeuristic ∷ Double    -- ^Step size.
                → InputSize -- ^Maximum input size.
                → Strategy a
linearHeuristic step maxSize = Strategy { nextInputSize = \n xs _ → return $ f n $ convertSamples xs
                                        , runStrategy   = id
                                        }
    where
      f ∷ Int → [(Double, Double)] → Maybe InputSize
      f n xs@(~((x, y) : _))
          | n ≤ 2     = Just (fromIntegral n)
          | otherwise = check (≤ maxSize) $
                          maybe ( let n' = ceiling x
                                      x' = ceiling (x ⋅ step)
                                  in if x' ≤ n' then n' + 1 else x'
                                )
                                (\n' → ceiling $ g n')
                                $ do (b, a) ← fitLinear xs
                                     return $ linearInv b a (y ⋅ step)
          where g x' | x' ≤ x     = x + 1
                     | x' > 2 ⋅ x = 2 ⋅ x
                     | otherwise  = x'

      convertSamples ∷ [Sample] → [(Double, Double)]
      convertSamples = map (fromIntegral *** statsMean2)

-------------------------------------------------------------------------------
-- Sensors
-------------------------------------------------------------------------------

-- |Function that measures some aspect of the execution of an action.
type Sensor α β = (Action α β) → α → IO Double

-- |Measures the execution time of an action.
--
--  Actions will be executed repeatedly until the cumulative time exceeds
--  minSampleTime milliseconds. The final result will be the cumulative time
--  divided by the number of iterations. In order to get sufficient precision
--  the minSampleTime should be set to at least a few times the time source's
--  precision. If you want to know only the execution time of the supplied
--  action and not the evaluation time of its input value you should ensure
--  that the input value is in head normal form.
timeSensor ∷ NFData β
           ⇒ IO t             -- ^Current time.
           → (t → t → Double) -- ^Time difference.
           → Double           -- ^Minimum run time (in milliseconds).
           → Sensor α β
timeSensor t d minSampleTime action x = go 1 0 0
    where
      go n totIter totCpuT =
          do -- Time n iterations of action applied on x.
             curCpuT ← timeIO t d n action x
             -- Calculate new cumulative values.
             let totCpuT'  = totCpuT + curCpuT
                 totIter'  = totIter + n
             if totCpuT' ≥ minSampleTime
               then let numIter = fromIntegral totIter'
                    in return $ totCpuT' / numIter
               else go (2 ⋅ n) totIter' totCpuT'

-- |Time the evaluation of an IO action.
timeIO ∷ NFData β
       ⇒ IO t             -- ^Measure current time.
       → (t → t → Double) -- ^Difference between measured times.
       → Int              -- ^Number of times the action is repeated.
       → Sensor α β
timeIO t d n f x = do tStart  ← t
                      strictReplicateM_ n $ f x
                      tEnd ← t
                      return $ d tEnd tStart

-- |Measures the CPU time that passes while executing an action.
cpuTimeSensor ∷ NFData β ⇒ Double → Sensor α β
cpuTimeSensor = timeSensor getCPUTime (\x y → picoToMilli $ x - y)

-- |Measures the wall clock time that passes while executing an action.
wallClockTimeSensor ∷ NFData β ⇒ Double → Sensor α β
wallClockTimeSensor = timeSensor getCurrentTime (\x y → 1000 ⋅ (realToFrac $ diffUTCTime x y))

-------------------------------------------------------------------------------
-- Measurement results
-------------------------------------------------------------------------------

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
