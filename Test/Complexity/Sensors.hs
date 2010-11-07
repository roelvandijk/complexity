{-# LANGUAGE UnicodeSyntax #-}

module Test.Complexity.Sensors 
    ( -- *Sensors
      Sensor
    , resultSensor
    , timeSensor
    , cpuTimeSensor
    , wallClockTimeSensor
    ) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import System.CPUTime        ( getCPUTime )

-- from base-unicode-symbols:
import Data.Ord.Unicode      ( (≥) )
import Data.Function.Unicode ( (∘) ) 
import Prelude.Unicode       ( (⋅) )

-- from complexity:
import Test.Complexity.Misc  ( strictReplicateM_, picoToMilli )
import Test.Complexity.Types ( Action, InputSize )

-- from deepseq:
import Control.DeepSeq       ( NFData )

-- from time:
import Data.Time.Clock       ( getCurrentTime, diffUTCTime )


-------------------------------------------------------------------------------
-- Sensors
-------------------------------------------------------------------------------


-- |Function that measures some aspect of the execution of an action.
type Sensor α β = (Action α β) → α → IO Double

-- |Directly \'measures\' the result of an action. 
resultSensor ∷ Sensor α InputSize
resultSensor action = fmap fromInteger ∘ action

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
