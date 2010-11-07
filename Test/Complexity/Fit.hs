{-# LANGUAGE GADTs
           , ScopedTypeVariables 
           , UnicodeSyntax 
  #-}

module Test.Complexity.Fit where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Prelude hiding ( replicate )

-- from base:
import Control.Monad         ( MonadPlus, mzero, guard )
import Data.Function         ( on )
import Data.List             ( minimumBy, sortBy )
import Data.Maybe            ( catMaybes )

-- from base-unicode-symbols:
import Data.Eq.Unicode       ( (≡) )
import Data.Function.Unicode ( (∘) ) 
import Prelude.Unicode       ( (⋅) )

-- from levmar:
import LevMar.Fitting        ( Nat, Z, S
                             , Model, Jacobian
                             , SizedList(..)
                             , Info(..), CovarMatrix
                             , LevMarable
                             , defaultOpts
                             , noLinearConstraints
                             , levmar
                             )
import NFunction             ( ($*) )
import SizedList             ( replicate )

-- from hstats:
import Math.Statistics       ( mean )

-- from complexity:
import Test.Complexity.Misc  ( applyMany )


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


fitLinear ∷ ∀ r. (Floating r, LevMarable r)
          ⇒ [(r, r)] → Maybe (r, r)
fitLinear samples = either (const Nothing) (Just ∘ convertResult) $
                      levmar linear
                             (Just linearJacob)
                             (estimateLinear samples)
                             samples
                             1000
                             defaultOpts
                             Nothing
                             Nothing
                             noLinearConstraints
                             Nothing
    where convertResult ∷ (SizedList N2 r, Info r, CovarMatrix N2 r) → (r, r)
          convertResult (b ::: a ::: Nil, _, _) = (b, a)

-------------------------------------------------------------------------------

type FitFunction r a = ( String
                       , [(a, r)] → Maybe (a → r)
                       , r
                       )

calcError ∷ Fractional r ⇒ (a → r) → [(a, r)] → r
calcError f samples = sum [ (y - f x) ^ (2 ∷ Int)
                          | (x, y) ← samples
                          ] / fromIntegral (length samples)


commonFits ∷ (Floating r, LevMarable r) ⇒ [FitFunction r r]
commonFits = [ ( "constant"
               , mkLevMarFit constant (Just constantJacob) estimateConstant
               , 1
               )
             , ( "logarithmic"
               , mkLevMarFit logarithmic Nothing estimateLogarithmic
               , 1
               )
             , ( "linear"
               , mkLevMarFit linear (Just linearJacob) estimateLinear
               , 1
               )
             , ( "loglinear"
               , mkLevMarFit logLinear Nothing estimateLogLinear
               , 1
               )
             , ( "quadratic"
               , mkLevMarFit quadratic (Just quadraticJacob) estimateQuadratic
               , 1
               )
             , ( "exponential"
               , mkLevMarFit exponential Nothing estimateExponential
               , 1
               )
             ]

commonFitNames = [ "constant"
                 , "logarithmic"
                 , "linear"
                 , "loglinear"
                 , "quadratic"
                 , "cubic"
                 , "exponential"
                 ]

mkLevMarFit ∷ (Nat n, Fractional r, LevMarable r)
            ⇒ Model n r a
            → Maybe (Jacobian n r a)
            → ([(a, r)] → SizedList n r)
            → [(a, r)]
            → Maybe (a → r)
mkLevMarFit m j e samples = either (const Nothing)
                                   (\(ps, info, _) → Just $ m $* ps)
                                   $ levmar m
                                            j
                                            (e samples)
                                            samples
                                            1000
                                            defaultOpts
                                            Nothing
                                            Nothing
                                            noLinearConstraints
                                            Nothing

fitAll ∷ (Fractional r, Ord r) ⇒ [FitFunction r α] → [(α, r)] → [(String, α → r, r)]
fitAll xs samples = let ys = do (n, f, w) ← xs
                                let (Just f') = f samples
                                    e = w ⋅ calcError f' samples
                                guard (e ≡ e)
                                return (n, f', e)
                    in sortBy (compare `on` (\(f, b, e) → e)) ys

-------------------------------------------------------------------------------
-- Formula's
-------------------------------------------------------------------------------

type N1 = S Z
type N2 = S N1
type N3 = S N2
type N4 = S N3


-- Constant

constant ∷ Model N1 r α
constant a _ = a

constantJacob ∷ Num r ⇒ Jacobian N1 r α
constantJacob _ _ = 0 ::: Nil

estimateConstant ∷ Floating r ⇒ [(α, r)] → SizedList N1 r
estimateConstant = f
    where f [] = 0 ::: Nil
          f ps = mean (map snd ps) ::: Nil


-- Logarithmic

logarithmic ∷ Floating r ⇒ Model N2 r r
logarithmic b a x = logBase b x + a

estimateLogarithmic ∷ Floating r ⇒ [(α, r)] → SizedList N2 r
estimateLogarithmic _ = replicate 0

-- Linear

linear ∷ Num r ⇒ Model N2 r r
linear b a x = b⋅x + a

linearJacob ∷ Num r ⇒ Jacobian N2 r r
linearJacob _ a x = x ::: constantJacob a x

estimateLinear ∷ Fractional r ⇒ [(r, r)] → SizedList N2 r
estimateLinear = f
    where f []  = zeros
          f [_] = zeros
          f ps  = maybe zeros
                        (\(a, b) → a ::: b ::: Nil)
                        $ ptsToLine (head ps) (last ps)
          zeros = replicate 0

-- Inverse Linear

linearInv ∷ Fractional r ⇒ Model N2 r r
linearInv b a y = (y - a) / b

-- Log Linear

logLinear ∷ Floating r ⇒ Model N2 r r
logLinear b a x = x ⋅ logBase b x + a

estimateLogLinear ∷ Floating r ⇒ [(α, r)] → SizedList N2 r
estimateLogLinear _ = replicate 0

-- Quadratic

quadratic ∷ Num r ⇒ Model N3 r r
quadratic c b a x = c⋅x⋅x + linear b a x

quadraticJacob ∷ Num r ⇒ Jacobian N3 r r
quadraticJacob _ b a x = x⋅x ::: linearJacob b a x

estimateQuadratic ∷ Floating r ⇒ [(α, r)] → SizedList N3 r
estimateQuadratic _ = replicate 0

-- Cubic

cubic ∷ Num r ⇒ Model N4 r r
cubic d c b a x = d⋅x⋅x⋅x + quadratic c b a x

cubicJacob ∷ Num r ⇒ Jacobian N4 r r
cubicJacob _ c b a x = x⋅x⋅x ::: quadraticJacob c b a x

estimateCubic ∷ Floating r ⇒ [(α, r)] → SizedList N4 r
estimateCubic _ = replicate 0

-- Exponential

exponential ∷ Floating r ⇒ Model N2 r r
exponential b a x = b ** (a ⋅ x)

estimateExponential ∷ Floating r ⇒ [(α, r)] → SizedList N2 r
estimateExponential _ = replicate 0

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

-- | Calculates the coefficients @a@ and @b@ of a linear equation @f x
-- = a*x + b@ given two points that lie on the line described by the
-- equation.
ptsToLine ∷ (MonadPlus m, Fractional α) ⇒ (α, α) → (α, α) → m (α, α)
ptsToLine (x1, y1) (x2, y2) | dx ≡ 0    = mzero
                            | otherwise = return ( (y2 - y1) / dx
                                                 , ((x2 ⋅ y1) - (x1 ⋅ y2)) / dx
                                                 )
    where dx = x2 - x1
