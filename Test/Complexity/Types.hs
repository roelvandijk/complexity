module Test.Complexity.Types where

import Test.Complexity.Statistics

type Action a  = a -> IO ()
type SizeGen a = Integer -> a

data SampleStats = SampleStats { inputSize :: Integer
                               , cpuTime   :: Stats
                               , wallTime  :: Stats
                               } deriving Show

data EvalStats = EvalStats { desc      :: String
                           , timeStats :: [SampleStats]
                           } deriving Show

