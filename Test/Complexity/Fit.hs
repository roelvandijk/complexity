{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Test.Complexity.Fit where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Control.Arrow         ( (&&&) )
import "base" Control.Monad         ( MonadPlus, mzero )
import "base" Data.Functor          ( (<$>) )
import "base" Data.Monoid           ( mempty )
import "base-unicode-symbols" Data.Eq.Unicode       ( (≡) )
import "base-unicode-symbols" Prelude.Unicode       ( (⋅) )
import qualified "levmar" Numeric.LevMar as LM
import "statistics" Statistics.Sample ( mean )
import "hmatrix" Data.Packed.Matrix ( fromRows )
import "hmatrix" Data.Packed.Vector ( Vector, toList, fromList, (@>) )
import qualified "vector" Data.Vector as V


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

type Model = LM.Params Double → Double → Double
type Jacobian = LM.Params Double → Double → Vector Double
type Point = (Double, Double)

fit ∷ Model
    → Maybe Jacobian
    → ([Point] → LM.Params Double) -- estimate
    → [Point]
    → Maybe (LM.Params Double) -- result
fit model jacob estimate samples =
    either (const Nothing)
           (\(params, _, _) → Just params)
           $ LM.levmar (mkLevmarModel model)
                       (fmap mkJacobModel jacob)
                       (estimate samples)
                       (fromList $ map snd samples)
                       1000
                       LM.defaultOpts
                       mempty
  where
    mkLevmarModel ∷ Model → LM.Model Double
    mkLevmarModel m = \ps → fromList $ map (\(x, _) → m ps x) samples

    mkJacobModel ∷ Jacobian → LM.Jacobian Double
    mkJacobModel j = \ps → fromRows $ map (\(x, _) → j ps x) samples

fitLinear ∷ [Point] → Maybe (Double, Double)
fitLinear samples = ((@> 0) &&& (@> 1))
                    <$> fit linear (Just linearJacob) estimateLinear samples

-------------------------------------------------------------------------------
-- Formula's
-------------------------------------------------------------------------------

-- Constant
constant ∷ Model
constant ps _ = let a = ps @> 0
                in a

constantJacob ∷ Jacobian
constantJacob _ _ = fromList [0]

estimateConstant ∷ [Point] → LM.Params Double
estimateConstant = f
    where f [] = fromList [0]
          f ps = fromList [mean $ V.fromList $ map snd ps]

-- Logarithmic
logarithmic ∷ Model
logarithmic ps x = let a = ps @> 0
                       b = ps @> 1
                   in logBase b x + a

-- Linear
linear ∷ Model
linear ps x = let a = ps @> 0
                  b = ps @> 1
              in a⋅x + b

linearJacob ∷ Jacobian
linearJacob _ x = fromList [x, 0]

estimateLinear ∷ [Point] → LM.Params Double
estimateLinear = f
       where f []  = zeros
             f [_] = zeros
             f ps  = maybe zeros
                           (\(a, b) → fromList [a, b])
                           $ ptsToLine (head ps) (last ps)
             zeros = fromList [0, 0]

-- Inverse Linear
linearInv ∷ Model
linearInv ps y = let a = ps @> 0
                     b = ps @> 1
                 in (y - b) / a

-- Log Linear

-- logLinear ∷ Floating r ⇒ Model N2 r r
-- logLinear b a x = x ⋅ logBase b x + a

-- estimateLogLinear ∷ Floating r ⇒ [(α, r)] → SizedList N2 r
-- estimateLogLinear _ = replicate 0

-- Quadratic
quadratic ∷ Model
quadratic ps x = let [a, b, c] = toList ps
                 in a⋅x⋅x + b⋅x + c

quadraticJacob ∷ Jacobian
quadraticJacob _ x = fromList [2⋅x, x, 0]

-- estimateQuadratic ∷ Floating r ⇒ [(α, r)] → SizedList N3 r
-- estimateQuadratic _ = replicate 0

-- Cubic

-- cubic ∷ Num r ⇒ Model N4 r r
-- cubic d c b a x = d⋅x⋅x⋅x + quadratic c b a x

-- cubicJacob ∷ Num r ⇒ Jacobian N4 r r
-- cubicJacob _ c b a x = x⋅x⋅x ::: quadraticJacob c b a x

-- estimateCubic ∷ Floating r ⇒ [(α, r)] → SizedList N4 r
-- estimateCubic _ = replicate 0

-- Exponential

exponential ∷ Model
exponential ps x = let [a, b] = toList ps
                   in a ** (b ⋅ x)

-- exponentialJacob ∷ Jacobian
-- exponentialJacob ps x = let [a, b] = T.toList ps
--                         in fromList [?, ?]

-- estimateExponential ∷ Floating r ⇒ [(α, r)] → SizedList N2 r
-- estimateExponential _ = replicate 0

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

-- | Calculates the coefficients @a@ and @b@ of a linear equation @f x
-- = a*x + b@ given two points that lie on the line described by the
-- equation.
ptsToLine ∷ (MonadPlus m) ⇒ Point → Point → m (Double, Double)
ptsToLine (x1, y1) (x2, y2) | dx ≡ 0    = mzero
                            | otherwise = return ( (y2 - y1) / dx
                                                 , ((x2 ⋅ y1) - (x1 ⋅ y2)) / dx
                                                 )
    where dx = x2 - x1


------------------------------------------------------------------------------

-- type FitFunction r a = ( String
--                        , [(a, r)] → Maybe (a → r)
--                        , r
--                        )

-- calcError ∷ Fractional r ⇒ (a → r) → [(a, r)] → r
-- calcError f samples = sum [ (y - f x) ^ (2 ∷ Int)
--                           | (x, y) ← samples
--                           ] / fromIntegral (length samples)


-- commonFits ∷ (Floating r, LevMarable r) ⇒ [FitFunction r r]
-- commonFits = [ ( "constant"
--                , mkLevMarFit constant (Just constantJacob) estimateConstant
--                , 1
--                )
             -- , ( "logarithmic"
             --   , mkLevMarFit logarithmic Nothing estimateLogarithmic
             --   , 1
             --   )
             -- , ( "linear"
             --   , mkLevMarFit linear (Just linearJacob) estimateLinear
             --   , 1
             --   )
             -- , ( "loglinear"
             --   , mkLevMarFit logLinear Nothing estimateLogLinear
             --   , 1
             --   )
             -- , ( "quadratic"
             --   , mkLevMarFit quadratic (Just quadraticJacob) estimateQuadratic
             --   , 1
             --   )
             -- , ( "exponential"
             --   , mkLevMarFit exponential Nothing estimateExponential
             --   , 1
             --   )
             -- ]

-- mkLevMarFit ∷ (Nat n, Fractional r, LevMarable r)
--             ⇒ Model n r a
--             → Maybe (Jacobian n r a)
--             → ([(a, r)] → SizedList n r)
--             → [(a, r)]
--             → Maybe (a → r)
-- mkLevMarFit m j e samples = either (const Nothing)
--                                    (\(ps, _, _) → Just $ m $* ps)
--                                    $ levmar m
--                                             j
--                                             (e samples)
--                                             samples
--                                             1000
--                                             defaultOpts
--                                             Nothing
--                                             Nothing
--                                             noLinearConstraints
--                                             Nothing

-- fitAll ∷ (Fractional r, Ord r) ⇒ [FitFunction r α] → [(α, r)] → [(String, α → r, r)]
-- fitAll xs samples = let ys = do (n, f, w) ← xs
--                                 let (Just f') = f samples
--                                     e = w ⋅ calcError f' samples
--                                 guard (e ≡ e)
--                                 return (n, f', e)
--                     in sortBy (compare `on` (\(_, _, e) → e)) ys
