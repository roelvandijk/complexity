{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Test.Complexity where

import Control.Monad               (liftM2, replicateM_)

import Control.Parallel            (pseq)
import Control.Parallel.Strategies (NFData, rnf, using)
import Data.Time.Clock             (getCurrentTime, diffUTCTime)
import System.CPUTime              (getCPUTime)
import System.Mem                  (performGC)

import Test.Complexity.Statistics
import Test.Complexity.Types

-------------------------------------------------------------------------------

data Test = forall a b. (NFData a, NFData b) => Test String (SizeGen a) (a -> b)


measureNs :: (NFData a) => String -> SizeGen a -> Action a -> Int -> [Integer] -> IO (Maybe EvalStats)
measureNs desc gen action numSamples ns = do xs <- mapM (\n -> measureAction gen action numSamples n 10 (2 ^ 12)) ns
                                             return $ fmap mkEvalStats $ sequence xs
    where
      mkEvalStats ts = EvalStats {desc = desc, timeStats = ts}

-- smartMeasure :: (Eval a) => String -- ^A description of what is being measured
--                          -> SizeGen a
--                          -> Action a
--                          -> Int    -- ^Number of samples
--                          -> Double -- ^Maximum measure time
--                          -> IO (Maybe EvalStats)
-- smartMeasure desc gen action numSamples maxTime = return Nothing


-- |Measure the time needed to evaluate an action when applied to an
--  input of size 'n'.
measureAction :: NFData a
              => SizeGen a -- ^Function that produces a value of size 'n'
              -> Action a  -- ^The action to measure
              -> Int       -- ^Number of samples
              -> Integer   -- ^Size of the input value (n)
              -> Double    -- ^Minimum run time (in milliseconds)
              -> Int       -- ^Maximum number of iterations per sample
              -> IO (Maybe SampleStats)
measureAction gen action numSamples inputSize minTime maxIter = fmap analyze $ measure
    where analyze :: [(Double, Double)] -> Maybe SampleStats
          analyze ts = let (cpuTimes, wallTimes) = unzip ts
                       in liftM2 (\c w -> SampleStats {inputSize = inputSize, cpuTime = c, wallTime = w})
                                 (stats cpuTimes)
                                 (stats wallTimes)

          measure :: IO [(Double, Double)]
          measure = let x  = gen inputSize `using` rnf
                    in x `pseq` (mapM (sample minTime maxIter action) $ replicate numSamples x)

sample :: Double           -- ^Minimum run time (in milliseconds)
       -> Int              -- ^Maximum number of iterations per sample
       -> Action a         -- ^The action to measure
       -> a                -- ^Input value
       -> IO (Double, Double)
sample minTime maxIter action x = go 1 0 0 0
    where go n totIter totCpuT totWallT =
              do -- Time n iterations of action applied on x.
                 (curCpuT, curWallT) <- timeIO action x n
                 let totCpuT'  = totCpuT  + curCpuT
                     totWallT' = totWallT + curWallT
                     totIter'  = totIter + n
                 if totCpuT' >= minTime || totIter' >= maxIter
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
                  replicateM_ n (f x)
                  -- Record the finish time.
                  endCPU  <- getCPUTime
                  endWall <- getCurrentTime
                  return ( picoToMilli $ endCPU - startCPU
                         , 1000 * (realToFrac $ diffUTCTime endWall startWall)
                         )

-------------------------------------------------------------------------------

picoToMilli :: Integer -> Double
picoToMilli p = (fromInteger $ p `div` 1000000) / 1000

-------------------------------------------------------------------------------

strictAction :: NFData b => (a -> b) -> (a -> IO ())
strictAction f = \x -> (f x `using` rnf) `pseq` return ()

-------------------------------------------------------------------------------

{-
-- |Creating values of size 'n'
class SizeGen a where
    mkValue :: Integer -> a

instance SizeGen () where
    mkValue = const ()

instance SizeGen Bool where
    mkValue 0 = False
    mkValue _ = True

instance SizeGen Int where
    mkValue = fromInteger

instance SizeGen Integer where
    mkValue = id

instance SizeGen a => SizeGen (Maybe a) where
    mkValue 0 = Nothing
    mkValue n = Just $ mkValue (n - 1)

instance (SizeGen l, SizeGen r) => SizeGen (Either l r) where
    mkValue n | odd n     = Left  $ mkValue n
              | otherwise = Right $ mkValue n

instance (SizeGen a, SizeGen b) => SizeGen (a, b) where
    mkValue n = let n' = n `div` 2
                in (mkValue n', mkValue n')

instance (SizeGen a, SizeGen b, SizeGen c) => SizeGen (a, b, c) where
    mkValue n = let n' = n `div` 3
                in (mkValue n', mkValue n', mkValue n')

instance (SizeGen a, SizeGen b, SizeGen c, SizeGen d) => SizeGen (a, b, c, d) where
    mkValue n = let n' = n `div` 4
                in (mkValue n', mkValue n', mkValue n', mkValue n')

newtype LinList a = LinList { unLinList :: [a] } deriving Show
instance SizeGen a => SizeGen (LinList a) where
    mkValue n = LinList $ genericReplicate n (mkValue 0)

newtype SqrList a = SqrList { unSqrList :: [a] } deriving Show
instance SizeGen a => SizeGen (SqrList a) where
    mkValue 0 = SqrList []
    mkValue n = SqrList $ map mkValue [0 .. n - 1]
-}
