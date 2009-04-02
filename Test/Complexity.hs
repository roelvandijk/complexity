{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Test.Complexity ( Action
                       , SizeGen
                       , SizeGenM

                       , Measurer
                       , Measurable
                       , MeasurementStats(..)
                       , SampleStats(..)
                       , Stats(..)

                       , measurable
                       , pureMeasurable

                       , measureNs
                       , smartMeasure
                       ) where

import Control.Parallel.Strategies (NFData)
import Data.Time.Clock             (UTCTime, getCurrentTime, diffUTCTime)
import Math.Statistics             (stddev, mean)
import System.CPUTime              (getCPUTime)
import System.Timeout              (timeout)
import Test.Complexity.Misc

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- |An Action is a function which can be measured.
type Action a b = a -> IO b

-- |A SizeGen produces a value of a certain size.
type SizeGen  a = Integer -> a
type SizeGenM a = Integer -> IO a

type Measurer = Measurable -> IO MeasurementStats

-- |Statistics about a measurement performed on many inputs.
data MeasurementStats = MeasurementStats { desc      :: String
                                         , timeStats :: [SampleStats]
                                         } deriving Show

-- |Statistics about the sampling of a single input value.
data SampleStats = SampleStats { inputSize :: Integer
                               , cpuTime   :: Stats
                                 -- ^CPU time statistics
                               , wallTime  :: Stats
                                 -- ^Wall clock time statistics
                               } deriving Show

data Stats = Stats { statsMin     :: Double
                   , statsMax     :: Double
                   , statsStdDev  :: Double
                   , statsMean    :: Double
                   , statsMean2   :: Double
                   -- ^Mean of all samples that lie within one
                   --  standard deviation from the mean.
                   , statsSamples :: Int
                   -- ^Number of samples from which these statistics
                   --  are derived.
                   } deriving Show

-- |Something that can be measured. Contains all information that is
--  necessary to perform a measurement.
data Measurable = forall a b. (NFData a, NFData b) => Measurable String (SizeGenM a) (Action a b)

measurable :: (NFData a, NFData b) => String -> SizeGenM a -> Action a b -> Measurable
measurable = Measurable

-- |Construct a Measurable from pure functions.
pureMeasurable :: (NFData a, NFData b) => String -> SizeGen a -> (a -> b) -> Measurable
pureMeasurable desc gen f= Measurable desc (return . gen) (return . f)

-------------------------------------------------------------------------------

-- Precondition: not $ null xs
stats :: [Double] -> Stats
stats xs = Stats { statsMin     = minimum xs
                 , statsMax     = maximum xs
                 , statsStdDev  = stddev_xs
                 , statsMean    = mean_xs
                 , statsMean2   = mean2_xs
                 , statsSamples = length xs
                 }
    where stddev_xs = stddev xs
          mean_xs   = mean xs
          mean2_xs  = let xs' = filter (\x -> diff mean_xs x < stddev_xs) xs
                      in if null xs'
                         then mean_xs
                         else mean xs'

-------------------------------------------------------------------------------

measureNs :: Int
          -> [Integer]
          -> Measurer
measureNs numSamples ns (Measurable desc gen action) =
    do xs <- mapM (measureAction gen action numSamples minSampleTime maxIter) ns
       return $ mkMeasurementStats xs
    where
      mkMeasurementStats ts = MeasurementStats {desc = desc, timeStats = ts}
      minSampleTime  = 10
      maxIter        = 2 ^ (16 :: Int)

smartMeasure :: Int       -- ^Number of samples
             -> Double    -- ^Minimum sample time (in CPU milliseconds)
             -> Double    -- ^Time increment coefficient
             -> Double    -- ^Maximum measure time in seconds (wall clock time)
             -> Integer   -- ^Maximum input size
             -> Measurer
smartMeasure numSamples minSampleTime timeInc maxMeasureTime maxN (Measurable desc gen action) =
    do t0 <- getCurrentTime
       x  <- measureAction gen action numSamples minSampleTime maxIter 0
       xs <- loop t0 x
       return $ mkMeasurementStats (reverse xs)
    where
      maxIter = 2 ^ (16 :: Int)

      mkMeasurementStats :: [SampleStats] -> MeasurementStats
      mkMeasurementStats ts = MeasurementStats {desc = desc, timeStats = ts}

      loop :: UTCTime -> SampleStats -> IO [SampleStats]
      loop startTime x = go x 1 [x]
          where
            go :: SampleStats -> Integer -> [SampleStats] -> IO [SampleStats]
            go prev n acc = do curTime <- getCurrentTime
                               let remainingTime = maxMeasureTime - (realToFrac $ diffUTCTime curTime startTime)
                               if remainingTime > 0 && n < maxN
                                 then do mx <- timeout (round $ remainingTime * 1e6)
                                                       $ measureAction gen action numSamples minSampleTime maxIter n
                                         maybe (return acc)
                                               (\x -> do let curTime  = statsMean2 $ cpuTime x
                                                             prevTime = statsMean2 $ cpuTime prev
                                                             dN = n - inputSize prev
                                                             dT = curTime  - prevTime
                                                             nextN = ceiling $ ((fromInteger dN) / dT) * (timeInc * curTime)
                                                             n' | nextN <= n    = n + dN
                                                                | nextN > 2 * n = 2 * n
                                                                | otherwise     = nextN
                                                         go x n' (x:acc)
                                               )
                                               mx
                                 else return acc

-------------------------------------------------------------------------------

-- |Measure the time needed to evaluate an action when applied to an
--  input of size 'n'.
measureAction :: (NFData a, NFData b)
              => SizeGenM a
              -> Action  a b
              -> Int       -- ^Number of samples
              -> Double    -- ^Minimum sample time (in CPU milliseconds)
              -> Int       -- ^Maximum number of iterations per sample
              -> Integer   -- ^Size of the input value (n)
              -> IO SampleStats
measureAction gen action numSamples minSampleTime maxIter inputSize = fmap analyze $ measure
    where
      analyze :: [(Double, Double)] -> SampleStats
      analyze ts = let (cpuTimes, wallTimes) = unzip ts
                   in SampleStats { inputSize = inputSize
                                  , cpuTime = stats cpuTimes
                                  , wallTime = stats wallTimes
                                  }

      measure :: IO [(Double, Double)]
      measure = gen inputSize >>=| \x ->
                  mapM (sample minSampleTime maxIter action)
                       $ replicate numSamples x

-- |Measure the execution time of an action.
--  Actions will be executed repeatedly until the cumulative CPU time
--  exceeds minSampleTime milliseconds or until the maximum number of
--  iterations is reached. The final result will be the cumulative CPU
--  and wall clock times divided by the number of iterations.  In
--  order to get sufficient precision the minSampleTime should be set
--  to at least a few times the cpuTimePrecision. If you want to know
--  only the execution time of the supplied action and not the
--  evaluation time of its input value you should ensure that the
--  input value is in head normal form.
sample :: NFData b
       => Double           -- ^Minimum run time (in milliseconds)
       -> Int              -- ^Maximum number of iterations per sample
       -> Action a b       -- ^The action to measure
       -> a                -- ^Input value
       -> IO (Double, Double)
sample minSampleTime maxIter action x = go 1 0 0 0
    where
      go n totIter totCpuT totWallT =
          do -- Time n iterations of action applied on x.
             (curCpuT, curWallT) <- timeIO action x n
             -- Calculate new cumulative values.
             let totCpuT'  = totCpuT  + curCpuT
                 totWallT' = totWallT + curWallT
                 totIter'  = totIter + n
             -- TODO: move check to front, totIter may not exceed maxIter
             if totCpuT' >= minSampleTime || totIter' >= maxIter
               then let numIter = fromIntegral totIter'
                    in return (totCpuT' / numIter, totWallT' / numIter)
               else go (2 * n) totIter' totCpuT' totWallT'

-- |Time the evaluation of an IO action.
timeIO :: NFData b
       => Action a b          -- ^The IO action to measure
       -> a                   -- ^Input value of the IO action
       -> Int                 -- ^Number of times the action is repeated
       -> IO (Double, Double) -- ^CPU- and wall clock time
timeIO f x n = do -- Record the start time.
                  startWall <- getCurrentTime
                  startCPU  <- getCPUTime
                  -- Run the action.
                  strictReplicateM_ n $ f x
                  -- Record the finish time.
                  endCPU  <- getCPUTime
                  endWall <- getCurrentTime
                  return ( picoToMilli $ endCPU - startCPU
                         , 1000 * (realToFrac $ diffUTCTime endWall startWall)
                         )

