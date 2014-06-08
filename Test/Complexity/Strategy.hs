{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PackageImports            #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE UnicodeSyntax             #-}

module Test.Complexity.Strategy
    (
    -- *Measurement strategy
      Strategy(..)
    , inputSizeFromList
    , linearHeuristic
    , limitSamples
    ) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import "base" Control.Monad ( guard )
import "base" Control.Applicative ( (<|>) )
import "base" Control.Arrow                  ( (***) )
import "base" Data.Functor ( (<$>) )
import "base-unicode-symbols" Data.Function.Unicode ( (∘) )
import "base-unicode-symbols" Data.Ord.Unicode      ( (≤) )
import "base-unicode-symbols" Prelude.Unicode       ( (⋅) )
import "this" Test.Complexity.Fit     ( fitLinear, linearInv )
import "this" Test.Complexity.Types   ( InputSize, Sample, stMean )
import "transformers" Control.Monad.IO.Class         ( MonadIO )
import "transformers" Control.Monad.Trans.State.Lazy ( StateT, evalStateT, get, put )
import qualified "hmatrix" Data.Packed.Vector as HM ( fromList )

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
data Strategy = ∀ m. MonadIO m
              ⇒ Strategy
                 { -- | Function which calculates the next
                   -- 'InputSize' to measure. Arguments are, in
                   -- order: the number of samples measured thus
                   -- far, a list of previously measured samples and
                   -- the remaining time in seconds.
                   nextInputSize ∷ Int → [Sample] → Double → m (Maybe InputSize)
                   -- | Run function which lifts the strategy monad
                   -- to IO.
                 , runStrategy ∷ (m [Sample] → IO [Sample])
                 }

-- | A strategy which produces input sizes from a given list.
--
-- When the list is consumed it will produce 'Nothing'.
inputSizeFromList ∷ [InputSize] → Strategy
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
limitSamples ∷ Int → Strategy → Strategy
limitSamples maxSamples (Strategy next run) =
    Strategy { nextInputSize = \n xs t → next n (take maxSamples xs) t
             , runStrategy   = run
             }

linearHeuristic ∷ Double    -- ^Step size.
                → InputSize -- ^Maximum input size.
                → Strategy
linearHeuristic step maxSize =
    Strategy { nextInputSize = \n xs _ → return $ f n $ convertSamples xs
             , runStrategy   = id
             }
  where
    f ∷ Int → [(Double, Double)] → Maybe InputSize
    f n xs@(~((x, y) : _))
        | n ≤ 2     = Just (fromIntegral n)
        | otherwise = do n' ← ((ceiling ∘ g) <$> nextX) <|> return noNextX
                         guard $ n' < maxSize
                         return n'
        where g ∷ Double → Double
              g x' | x' ≤ x     = x + 1
                   | x' > 2 ⋅ x = 2 ⋅ x
                   | otherwise  = x'

              noNextX ∷ InputSize
              noNextX | x' ≤ n'   = n' + 1
                      | otherwise = x'
                where
                  n' = ceiling x
                  x' = ceiling (x ⋅ step)

              nextX ∷ Maybe Double
              nextX = do (a, b) ← fitLinear xs
                         return $ linearInv (HM.fromList [a, b]) (y ⋅ step)


    convertSamples ∷ [Sample] → [(Double, Double)]
    convertSamples = map (fromIntegral *** stMean)
