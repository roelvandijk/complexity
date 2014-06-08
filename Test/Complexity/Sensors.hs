{-# LANGUAGE PackageImports #-}
{-# LANGUAGE UnicodeSyntax  #-}

module Test.Complexity.Sensors
    ( -- *Sensors
      Sensor
    , criterionSensor
    ) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import "this" Test.Complexity.Types ( Action )
import "criterion" Criterion ( Benchmarkable, runBenchmark )
import "criterion" Criterion.Config ( Config )
import "criterion" Criterion.Monad ( withConfig )
import "criterion" Criterion.Environment ( Environment )
import "vector" Data.Vector.Unboxed ( Vector )

-------------------------------------------------------------------------------
-- Sensors
-------------------------------------------------------------------------------


-- |Function that measures some aspect of the execution of an action.
type Sensor α β = Action α β → α → IO (Vector Double)

criterionSensor ∷ (Benchmarkable β) ⇒ Environment → Config → Sensor α β
criterionSensor env conf action x = withConfig conf $ runBenchmark env $ action x
