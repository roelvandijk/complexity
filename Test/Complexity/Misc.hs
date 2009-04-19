{-# OPTIONS_HADDOCK hide #-}

module Test.Complexity.Misc ( (>>=|)
                            , (>>|)
                            , strictReplicateM_
                            , diff
                            , picoToMilli
                            ) where

import Control.Parallel.Strategies (NFData, rnf, ($|))

-- |Very strict monadic bind
(>>=|) :: (Monad m, NFData a) => m a -> (a -> m b) -> m b
m >>=| f = m >>= f $| rnf

(>>|) :: (Monad m, NFData a) => m a -> m b -> m b
m1 >>| m2 = m1 >>=| const m2

strictReplicateM_ :: NFData a => Int -> IO a  -> IO ()
strictReplicateM_ n a = go n
    where go 0 = return ()
          go n = a >>| go (n - 1)

-- |Difference
diff :: Num a => a -> a -> a
diff x y = abs $ x - y

picoToMilli :: Integer -> Double
picoToMilli p = (fromInteger p) / 1e9
