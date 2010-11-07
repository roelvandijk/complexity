{-# LANGUAGE UnicodeSyntax #-}

module Test.Complexity.Types 
    ( Action
    , InputGen
    , InputSize
    ) where

-------------------------------------------------------------------------------
-- Measurement subject
-------------------------------------------------------------------------------

-- |An Action is a function of which aspects of its execution can be measured.
type Action α β = α → IO β

-- |A input generator produces a value of a certain size.
type InputGen α = InputSize → α

-- |The size of an input on which an action is applied.
type InputSize = Integer

