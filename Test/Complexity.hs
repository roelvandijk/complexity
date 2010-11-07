{-|
This module provides a collection of functions that enable you to
measure the algorithmic complexity of arbitrary functions.

Let's say you want to measure the time complexity of 'qsort':

@
  qsort :: Ord a => [a] -> [a]
  qsort []     = []
  qsort (x:xs) = qsort (filter (\< x) xs) ++ [x] ++ qsort (filter (>= x) xs)
@

We want to now the time complexity of 'qsort' in terms of the size of
its 'InputSize' \'n\'. First we have to express what \'n\' is. We do this by
writing an 'InputGen':

@
  -- Very simple pseudo random number generator.
  pseudoRnd :: Int -> Int -> Int -> Int -> [Int]
  pseudoRnd p1 p2 n d = iterate (\x -> (p1 * x + p2) `mod` n) d
@

@
  genIntList :: 'InputGen' [Int]
  genIntList n = take (fromInteger n) $ pseudoRnd 16807 0 (2 ^ 31 - 1) 79
@

The function 'genIntList' now generates a pseudo random list of Ints
of length \'n\'.

Next we have to specify what aspect of 'qsort' we want to
measure. Since we are interested in the time complexity we use a CPU
time sensor:

@
  mySensor = 'cpuTimeSensor' 10
@

The 'cpuTimeSensor' is a 'Sensor' which measures CPU time. It takes
one argument which is a time in milliseconds. This is the minimum
execution time for an 'Action' which is measured. If the action doesn't
take more than 10 ms to execute it will be repeated until it
does. This allows us to measure actions which execute much faster than
the minimum measurable CPU time difference.

Now we can create an 'Experiment':

@
  expQSort = 'pureExperiment' \"quicksort\" mySensor genIntList qsort
@

This is an experiment which measures the CPU time it takes to apply
the function 'qsort' on an input generate by 'genIntList'.

Before you can perform the experiment you need to decide which input
sizes you want to measure and when to stop. These ideas are contained
in a 'Strategy'. We'll use the 'simpleLinearHeuristic':

@
  myStrategy = 'simpleLinearHeuristic' 1.1 10^5
@

This strategy looks at the last two points to decide which input size
to measure next. It picks a point where it thinks the measured value
will be 1.1 times the last measured value. It will stop if the input
size exceeds 10^5 to prevent running out of memory.

Now we can finally perform the experiment:

@
  stats <- 'performExperiment' myStrategy 10 15 expQSort
@

The experiment will take 10 samples per input size and it will run for
15 seconds. The result is a bunch of 'MeasurementStats'. You can now
print these statistics to stdout or show them in a nice graph:

@
  'printStats'     [stats]
  'showStatsChart' [stats]
@

Looking at the type signatures of these function you'll notice that
they accept a list of 'MeasurementStats'. This means you can compare
multiple experiments.

Let's compare 'qsort' to the build in 'Data.List.sort'. This time
we'll use some convenient utility functions to more easily setup and
perform an experiment.

@
  expSorts = [ 'pureExperiment' \"qsort\"          mySensor genIntList qsort
             , 'pureExperiment' \"Data.List.sort\" mySensor genIntList 'sort'
             ]
  'simpleSmartMeasure' 1.1 10^5 10 20 expSorts
@

The utility function 'simpleSmartMeasure' uses the
'simpleLinearHeuristic' strategy by default. The first to arguments
are passed to the heuristic. We again choose to take 10 samples per
input size. The total measurement time is increased to 20 seconds, but
it is now used to measure two functions instead of one. The time is
divided evenly and each function gets 10 seconds. The last argument is
a list of experiments. After 20 seconds you'll get a nice graph
comparing the complexity of the two sorting algorithms.

-}

module Test.Complexity
    ( module Test.Complexity.Config
    , module Test.Complexity.Experiment
    , module Test.Complexity.Main
    , module Test.Complexity.Monad
    , module Test.Complexity.Pretty
    , module Test.Complexity.Results
    , module Test.Complexity.Sensors
    , module Test.Complexity.Strategy
    , module Test.Complexity.Types
    , module Test.Complexity.Utils
    ) where

import Test.Complexity.Config
import Test.Complexity.Experiment
import Test.Complexity.Main
import Test.Complexity.Monad
import Test.Complexity.Pretty
import Test.Complexity.Results
import Test.Complexity.Sensors
import Test.Complexity.Strategy
import Test.Complexity.Types
import Test.Complexity.Utils
