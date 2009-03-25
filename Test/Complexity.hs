{-# LANGUAGE ScopedTypeVariables       #-}

module Test.Complexity where

import Control.Monad               (replicateM_)
import Control.Parallel            (pseq)
import Control.Parallel.Strategies (NFData, rnf, using)
import Data.Time.Clock             (UTCTime, getCurrentTime, diffUTCTime)
import System.CPUTime              (getCPUTime)
import System.Timeout              (timeout)
import Math.Statistics             (stddev, mean)

-- import System.Mem (performGC)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- |An @Action is a function which can be measured.
type Action a  = a -> IO ()

-- |A @SizeGen produces a value of a certain size.
type SizeGen a = Integer -> IO a

-- |Statistics about the sampling of a single input value.
data SampleStats = SampleStats { inputSize :: Integer
                               , cpuTime   :: Stats
                               , wallTime  :: Stats
                               } deriving Show

-- |Statistics about a measurement performed on many input sizes.
data EvalStats = EvalStats { desc      :: String
                           , timeStats :: [SampleStats]
                           } deriving Show

data Stats = Stats { stMin     :: Double
                   , stMax     :: Double
                   , stStdDev  :: Double
                   , stMean    :: Double
                   , stSamples :: Int
                   } deriving Show

-------------------------------------------------------------------------------

-- Precondition: not $ null xs
stats :: [Double] -> Stats
stats xs = Stats { stMin     = minimum xs
                 , stMax     = maximum xs
                 , stStdDev  = stddev  xs
                 , stMean    = mean    xs
                 , stSamples = length  xs
                 }

-------------------------------------------------------------------------------

measureNs :: NFData  a
          => String
          -> SizeGen a
          -> Action  a
          -> Int
          -> [Integer]
          -> IO EvalStats
measureNs desc gen action numSamples ns =
    do xs <- mapM (measureAction gen action numSamples minSampleTime maxIter) ns
       return $ mkEvalStats xs
    where
      mkEvalStats ts = EvalStats {desc = desc, timeStats = ts}
      minSampleTime  = 10
      maxIter        = 2 ^ (12 :: Int)

smartMeasure :: NFData  a
             => String
             -> SizeGen a
             -> Action  a
             -> Int       -- ^Number of samples
             -> Double    -- ^Minimum sample time (in CPU milliseconds)
             -> Int       -- ^Maximum number of iterations per sample
             -> Double    -- ^Time increment coefficient
             -> Double    -- ^Maximum measure time in seconds (wall clock time)
             -> IO (EvalStats)
smartMeasure desc gen action numSamples minSampleTime maxIter timeInc maxMeasureTime =
    do t0 <- getCurrentTime
       x  <- measureAction gen action numSamples minSampleTime maxIter 0
       xs <- loop t0 x
       return $ mkEvalStats (reverse xs)
    where
      mkEvalStats :: [SampleStats] -> EvalStats
      mkEvalStats ts = EvalStats {desc = desc, timeStats = ts}

      loop :: UTCTime -> SampleStats -> IO [SampleStats]
      loop startTime x = go x 1 [x]
          where
            go :: SampleStats -> Integer -> [SampleStats] -> IO [SampleStats]
            go prev n acc = do curTime <- getCurrentTime
                               let remainingTime = maxMeasureTime - (realToFrac $ diffUTCTime curTime startTime)
                               if remainingTime > 0
                                 then do mx <- timeout (round $ remainingTime * 1e6)
                                                       $ measureAction gen action numSamples minSampleTime maxIter n
                                         maybe (return acc)
                                               (\x -> do let curTime  = stMean $ cpuTime x
                                                             prevTime = stMean $ cpuTime prev
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


-- |Measure the time needed to evaluate an action when applied to an
--  input of size 'n'.
measureAction :: NFData  a
              => SizeGen a
              -> Action  a
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
      measure = do x <- gen inputSize
                   (x `using` rnf) `pseq` ( mapM (sample minSampleTime maxIter action)
                                                 $ replicate numSamples x
                                          )

-- |Measure the execution time of an action.
--  Actions will be executed repeatedly until the cumulative CPU time
--  exceeds @minSampleTime milliseconds or until the maximum number of
--  iterations is reached. The final result will be the cumulative CPU
--  and wall clock times divided by the number of iterations.
--  In order to get sufficient precision the @minSampleTime should be
--  set to at least a few times the @cpuTimePrecision. If you want to
--  know only the execution time of the supplied action and not the
--  evaluation time of its input value you should ensure that the
--  input value is in head normal form.
sample :: Double           -- ^Minimum run time (in milliseconds)
       -> Int              -- ^Maximum number of iterations per sample
       -> Action a         -- ^The action to measure
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
             if totCpuT' >= minSampleTime || totIter' >= maxIter
               then let numIter = fromIntegral totIter'
                    in return (totCpuT' / numIter, totWallT' / numIter)
               else go (2 * n) totIter' totCpuT' totWallT'


-- |Time the evaluation of an IO action.
timeIO :: Action a            -- ^The IO action to measure
       -> a                   -- ^Input value of the IO action
       -> Int                 -- ^Number of times the action is repeated
       -> IO (Double, Double) -- ^CPU- and wall clock time
timeIO f x n = do -- Record the start time.
                  startWall <- getCurrentTime
                  startCPU  <- getCPUTime
                  -- Run the action.
                  replicateM_ n $ f x
                  -- Record the finish time.
                  endCPU  <- getCPUTime
                  endWall <- getCurrentTime
                  return ( picoToMilli $ endCPU - startCPU
                         , 1000 * (realToFrac $ diffUTCTime endWall startWall)
                         )

-------------------------------------------------------------------------------

picoToMilli :: Integer -> Double
picoToMilli p = (fromInteger p) / 1e9

-------------------------------------------------------------------------------

strictAction :: NFData b => (a -> b) -> (a -> IO ())
strictAction f = \x -> (f x `using` rnf) `pseq` return ()
