{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LiberalTypeSynonyms       #-}
{-# LANGUAGE PackageImports            #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Test.Complexity ( -- *Measurement subject
                         Action
                       , InputGen
                       , InputSize

                         -- *Experiments
                       , Experiment
                       , experiment
                       , pureExperiment
                       , performExperiment

                         -- *Measurement strategy
                       , Strategy
                       , simpleLinearHeuristic
                       , inputSizeFromList

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

-- Package-qualified import because of Chart, which exports stuff from mtl.
import "transformers" Control.Monad.Trans (MonadIO, liftIO)

import Control.Monad                  (liftM)
import Control.Monad.Trans.State.Lazy (StateT, evalStateT, get, put)
import Control.Parallel.Strategies    (NFData)
import Data.List                      (genericReplicate, sortBy)
import Data.Function                  (on)
import Data.Time.Clock                (getCurrentTime, diffUTCTime)
import Math.Statistics                (stddev, mean)
import System.CPUTime                 (getCPUTime)
import System.Timeout                 (timeout)
import Test.Complexity.Misc

-------------------------------------------------------------------------------
-- Measurement subject
-------------------------------------------------------------------------------

-- |An Action is a function of which aspects of its execution can be measured.
type Action a b = a -> IO b

-- |A input generator produces a value of a certain size.
type InputGen a = InputSize -> a

-- |The size of an input on which an action is applied.
type InputSize = Integer

-------------------------------------------------------------------------------
-- Experiments
-------------------------------------------------------------------------------

-- |A method of investigating the causal relationship between the size
--  of the input of an action and some aspect of the execution of the action.
data Experiment = forall a b. NFData a =>
                  Experiment String (Sensor a b) (InputGen (IO a)) (Action a b)

experiment :: NFData a => String -> (Sensor a b) -> InputGen (IO a) -> (Action a b) -> Experiment
experiment = Experiment

pureExperiment :: NFData a => String -> (Sensor a b) -> InputGen a -> (a -> b) -> Experiment
pureExperiment desc sensor gen f = experiment desc sensor (return . gen) (return . f)

-------------------------------------------------------------------------------

performExperiment :: Strategy MeasurementStats
        -> Integer -- ^Number of samples per input size.
        -> Double  -- ^Maximum measure time in seconds (wall clock time).
        -> Experiment
        -> IO MeasurementStats
performExperiment (Strategy next run) numSamples maxMeasureTime (Experiment desc sensor gen action) =
    do startTime <- getCurrentTime

       let measureLoop xs = do curTime <- liftIO getCurrentTime
                               let elapsedTime = diffUTCTime curTime startTime
                               let remTime = maxMeasureTime - realToFrac elapsedTime

                               n' <- next xs remTime
                               case n' of
                                 Just n | remTime > 0 -> liftIO (timeout (round $ remTime * 1e6) $ measureSample n)
                                                         >>= maybe (return xs) (\x -> measureLoop (x:xs))
                                 _ -> return xs

       run $ liftM ((MeasurementStats desc) . (sortBy (compare `on` fst)))
                   $ measureLoop []
    where
      measureSample :: InputSize -> IO Sample
      measureSample = measureAction gen action sensor numSamples

-- |Measure the time needed to evaluate an action when applied to an input of
--  size 'n'.
measureAction :: NFData a
              => InputGen (IO a) -> Action a b -> Sensor a b -> Integer -> InputSize -> IO Sample
measureAction gen action sensor numSamples inputSize = fmap (\ys -> (inputSize, calculateStats ys))
                                                            measure
    where
      measure :: IO [Double]
      measure = gen inputSize >>=| \x -> mapM (sensor action) $ genericReplicate numSamples x

-------------------------------------------------------------------------------
-- Measurement strategy
-------------------------------------------------------------------------------

data Strategy a = forall m. MonadIO m =>
                  Strategy ([Sample] -> Double -> m (Maybe InputSize))
                            (m a -> IO a)


simpleLinearHeuristic :: Double -> InputSize -> Strategy a
simpleLinearHeuristic step maxSize = Strategy (\xs _ -> return $ f xs) id
    where
    f :: [Sample] -> Maybe InputSize
    f xs | n < maxSize = Just n
         | otherwise   = Nothing
        where
          n = simple step xs

          simple _    []                                = 0
          simple _    [_]                               = 1
          simple step ((x2,y2):(x1,y1):_) | x3 <= x2    = x2 + dx
                                          | x3 > 2 * x2 = 2 * x2
                                          | otherwise   = x3
              where
                t2 = statsMean2 $ y2
                t1 = statsMean2 $ y1
                dx = x2 - x1
                dt = t2 - t1
                x3 = ceiling $ (fromInteger dx / dt) * (step * t2)

inputSizeFromList :: [InputSize] -> Strategy a
inputSizeFromList ns = Strategy (\_ _ -> m) (\s -> evalStateT s ns)
    where
      m :: StateT [InputSize] IO (Maybe InputSize)
      m = do xs <- get
             case xs of
               []      -> return Nothing
               (x:xs') -> do put xs'
                             return $ Just x

-------------------------------------------------------------------------------
-- Sensors
-------------------------------------------------------------------------------

-- |Function that measures some aspect of the execution of an action.
type Sensor a b = (Action a b) -> a -> IO Double

-- |Measure the execution time of an action.
--
--  Actions will be executed repeatedly until the cumulative time exceeds
--  minSampleTime milliseconds. The final result will be the cumulative time
--  divided by the number of iterations. In order to get sufficient precision
--  the minSampleTime should be set to at least a few times the time source's
--  precision. If you want to know only the execution time of the supplied
--  action and not the evaluation time of its input value you should ensure
--  that the input value is in head normal form.
timeSensor :: NFData b
           => IO t               -- ^Current time.
           -> (t -> t -> Double) -- ^Time difference.
           -> Double             -- ^Minimum run time (in milliseconds).
           -> Sensor a b
timeSensor t d minSampleTime action x = go 1 0 0
    where
      go n totIter totCpuT =
          do -- Time n iterations of action applied on x.
             curCpuT <- timeIO t d n action x
             -- Calculate new cumulative values.
             let totCpuT'  = totCpuT  + curCpuT
                 totIter'  = totIter + n
             if totCpuT' >= minSampleTime
               then let numIter = fromIntegral totIter'
                    in return $ totCpuT' / numIter
               else go (2 * n) totIter' totCpuT'

-- |Time the evaluation of an IO action.
timeIO :: NFData b
      => IO t               -- ^Measure current time.
      -> (t -> t -> Double) -- ^Difference between measured times.
      -> Int                -- ^Number of times the action is repeated.
      -> Sensor a b
timeIO t d n f x = do tStart  <- t
                      strictReplicateM_ n $ f x
                      tEnd <- t
                      return $ d tEnd tStart

cpuTimeSensor :: NFData b => Double -> Sensor a b
cpuTimeSensor = timeSensor getCPUTime (\x y -> picoToMilli $ x - y)

wallClockTimeSensor :: NFData b => Double -> Sensor a b
wallClockTimeSensor = timeSensor getCurrentTime (\x y -> 1000 * (realToFrac $ diffUTCTime x y))

-------------------------------------------------------------------------------
-- Measurement results
-------------------------------------------------------------------------------

-- |Statistics about a measurement performed on many inputs.
data MeasurementStats = MeasurementStats { msDesc    :: String
                                         , msSamples :: [Sample]
                                         } deriving Show

-- |Statistics about the sampling of a single input value.
type Sample = (InputSize, Stats)

data Stats = Stats { statsMin     :: Double -- ^Minimum value.
                   , statsMax     :: Double -- ^Maximum value.
                   , statsStdDev  :: Double -- ^Standard deviation
                   , statsMean    :: Double -- ^Arithmetic mean.
                   , statsMean2   :: Double
                   -- ^Mean of all samples that lie within one
                   --  standard deviation from the mean.
                   , statsSamples :: [Double] -- ^Samples from which these statistics are derived.
                   } deriving Show

-------------------------------------------------------------------------------
-- Misc
-------------------------------------------------------------------------------

-- Precondition: not $ null xs
calculateStats :: [Double] -> Stats
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
          inStddev = filter (\x -> diff mean_xs x < stddev_xs) xs
