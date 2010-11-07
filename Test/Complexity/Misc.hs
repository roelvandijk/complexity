{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_HADDOCK hide #-}

module Test.Complexity.Misc ( (>>=|)
                            , (>>|)
                            , strictReplicateM_
                            , diff
                            , picoToMilli
                            , check
                            , applyMany
                            ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------


-- from base:
import Control.Monad               ( Functor, MonadPlus, mzero )

-- from deepseq:
import Control.DeepSeq             ( NFData )

-- from parallel:
import Control.Parallel.Strategies ( ($|), rdeepseq )


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


-- |Very strict monadic bind
(>>=|) ∷ (Monad m, NFData α) ⇒ m α → (α → m β) → m β
m >>=| f = m >>= f $| rdeepseq

(>>|) ∷ (Monad m, NFData α) ⇒ m α → m β → m β
m1 >>| m2 = m1 >>=| const m2

strictReplicateM_ ∷ NFData α ⇒ Int → IO α → IO ()
strictReplicateM_ n a = go n
    where go 0 = return ()
          go n = a >>| go (n - 1)

-- |Difference
diff ∷ Num α ⇒ α → α → α
diff x y = abs $ x - y

picoToMilli ∷ Integer → Double
picoToMilli p = (fromInteger p) / 1e9

check ∷ MonadPlus m ⇒ (α → Bool) → α → m α
check p x | p x       = return x
          | otherwise = mzero

-- |Apply a bunch of functions on a single value producing a bunch of
-- results.
applyMany ∷ Functor f ⇒ f (α → β) → α → f β
applyMany fs x = fmap ($ x) fs


