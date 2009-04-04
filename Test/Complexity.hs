{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE PackageImports            #-}

module Test.Complexity ( Action
                       , InputGen
                       , InputGenM
                       , InputSize
                       , NextInputSize
                       , NextInputSizeM
                       , Measurable
                       , measurable
                       , pureMeasurable

                       , MeasurementStats(..)
                       , SampleStats(..)
                       , Stats(..)

                       , measure
                       , measureNs
                       , smartMeasure
                       ) where

-- Package-qualified import because of Chart, which exports stuff from mtl.
import "transformers" Control.Monad.Trans  (MonadIO, liftIO)

import Control.Monad                  (liftM)
import Control.Monad.Trans.State.Lazy (StateT, evalStateT, get, put)
import Control.Parallel.Strategies    (NFData)
import Data.List                      (sortBy)
import Data.Function                  (on)
import Data.Time.Clock                (getCurrentTime, diffUTCTime)
import Math.Statistics                (stddev, mean)
import System.CPUTime                 (getCPUTime)
import System.Timeout                 (timeout)
import Test.Complexity.Misc

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- |An Action is a function of which aspects of its execution can be measured.
type Action a b = a -> IO b

-- |A InputGen produces a value of a certain size.
type InputGen  a = InputSize -> a
type InputGenM a = InputSize -> IO a

-- |The size of an input on which an action is applied.
type InputSize = Integer

-- |Something that can be measured. Contains all information that is necessary
--  to perform a measurement.
data Measurable = forall a b. (NFData a, NFData b) => Measurable String (InputGenM a) (Action a b)

measurable :: (NFData a, NFData b) => String -> InputGenM a -> Action a b -> Measurable
measurable = Measurable

-- |Construct a Measurable from a pure function.
pureMeasurable :: (NFData a, NFData b) => String -> InputGen a -> (a -> b) -> Measurable
pureMeasurable desc gen f = Measurable desc (return . gen) (return . f)

-- |Function that gives the size of the next input on which the action that is
--  being measured should be applied.
type NextInputSizeM m = [SampleStats] -> Double -> m (Maybe InputSize)
type NextInputSize    = [SampleStats] -> Double -> Maybe InputSize

liftNextInputSize :: Monad m => NextInputSize -> NextInputSizeM m
liftNextInputSize nis xs remTime = return $ nis xs remTime

-- |Statistics about a measurement performed on many inputs.
data MeasurementStats = MeasurementStats { desc      :: String
                                         , timeStats :: [SampleStats]
                                         } deriving Show

-- |Statistics about the sampling of a single input value.
data SampleStats = SampleStats { inputSize :: InputSize
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

smartMeasure :: Double    -- ^Time increment step size
             -> InputSize -- ^Maximum input size
             -> Int       -- ^Number of samples per input size
             -> Double    -- ^Minimum sample time (in CPU milliseconds)
             -> Double    -- ^Maximum measure time in seconds (wall clock time)
             -> Measurable
             -> IO MeasurementStats
smartMeasure step maxSize = measure (liftNextInputSize $ simpleLinearHeuristic step maxSize) id

simpleLinearHeuristic :: Double -> InputSize -> NextInputSize
simpleLinearHeuristic step maxSize xs r | n < maxSize = Just n
                                        | otherwise   = Nothing
    where
      n = simple step xs r

      simple _    []        _                  = 0
      simple _    [_]       _                  = 1
      simple step (x1:x2:_) _ | nextN <= n1    = n1 + dN
                              | nextN > 2 * n1 = 2 * n1
                              | otherwise      = nextN
          where
            t1 = statsMean2 $ cpuTime x1
            t2 = statsMean2 $ cpuTime x2
            n1 = inputSize x1
            n2 = inputSize x2
            dN = n1 - n2
            dT = t1 - t2
            nextN = ceiling $ (fromInteger dN / dT) * (step * t1)

measureNs :: [InputSize]
          -> Int    -- ^Number of samples per input size
          -> Double -- ^Minimum sample time (in CPU milliseconds)
          -> Double -- ^Maximum measure time in seconds (wall clock time)
          -> Measurable
          -> IO MeasurementStats
measureNs ns = measure nextFromList $ \s -> evalStateT s ns
    where
      nextFromList :: NextInputSizeM (StateT [InputSize] IO)
      nextFromList _ _ = do ns <- get
                            case ns of
                              []      -> return Nothing
                              (n:ns') -> do put ns'
                                            return $ Just n

-------------------------------------------------------------------------------

measure :: forall m. MonadIO m
        => NextInputSizeM m
        -> (m MeasurementStats -> IO MeasurementStats)
        -> Int       -- ^Number of samples per input size
        -> Double    -- ^Minimum sample time (in CPU milliseconds)
        -> Double    -- ^Maximum measure time in seconds (wall clock time)
        -> Measurable
        -> IO MeasurementStats
measure next run numSamples minSampleTime maxMeasureTime (Measurable desc gen action) =
    do startTime <- getCurrentTime

       let measureLoop :: [SampleStats] -> m [SampleStats]
           measureLoop xs = do curTime <- liftIO getCurrentTime
                               let elapsedTime = diffUTCTime curTime startTime
                               let remTime = maxMeasureTime - realToFrac elapsedTime

                               n' <- next xs remTime
                               case n' of
                                 Just n | remTime > 0 ->
                                            liftIO (timeout (round $ remTime * 1e6) $ measureSample n)
                                            >>= maybe (return xs) (\x -> measureLoop (x:xs))
                                 _ -> return xs

       run $ liftM ((MeasurementStats desc) . (sortBy (compare `on` inputSize))) $ measureLoop []
    where
      measureSample :: InputSize -> IO SampleStats
      measureSample = measureAction gen action numSamples minSampleTime

-------------------------------------------------------------------------------

-- |Measure the time needed to evaluate an action when applied to an input of
--  size 'n'.
measureAction :: (NFData a, NFData b)
              => InputGenM a
              -> Action a b
              -> Int       -- ^Number of samples
              -> Double    -- ^Minimum sample time (in CPU milliseconds)
              -> InputSize -- ^Size of the input value (n)
              -> IO SampleStats
measureAction gen action numSamples minSampleTime inputSize = fmap analyze $ measure
    where
      analyze :: [(Double, Double)] -> SampleStats
      analyze ts = let (cpuTimes, wallTimes) = unzip ts
                   in SampleStats { inputSize = inputSize
                                  , cpuTime   = stats cpuTimes
                                  , wallTime  = stats wallTimes
                                  }

      measure :: IO [(Double, Double)]
      measure = gen inputSize >>=| \x ->
                  mapM (sample minSampleTime action)
                       $ replicate numSamples x

-- |Measure the execution time of an action.
--
--  Actions will be executed repeatedly until the cumulative CPU time exceeds
--  minSampleTime milliseconds or until the maximum number of iterations is
--  reached. The final result will be the cumulative CPU and wall clock times
--  divided by the number of iterations.  In order to get sufficient precision
--  the minSampleTime should be set to at least a few times the
--  cpuTimePrecision. If you want to know only the execution time of the
--  supplied action and not the evaluation time of its input value you should
--  ensure that the input value is in head normal form.
sample :: NFData b
       => Double           -- ^Minimum run time (in milliseconds)
       -> Action a b       -- ^The action to measure
       -> a                -- ^Input value
       -> IO (Double, Double)
sample minSampleTime action x = go 1 0 0 0
    where
      go n totIter totCpuT totWallT =
          do -- Time n iterations of action applied on x.
             (curCpuT, curWallT) <- timeIO action x n
             -- Calculate new cumulative values.
             let totCpuT'  = totCpuT  + curCpuT
                 totWallT' = totWallT + curWallT
                 totIter'  = totIter + n
             if totCpuT' >= minSampleTime
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

