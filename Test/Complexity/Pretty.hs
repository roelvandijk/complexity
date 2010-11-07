{-# LANGUAGE RecordWildCards 
           , UnicodeSyntax
  #-}

module Test.Complexity.Pretty ( prettyStats
                              , printStats
                              ) where

import Data.Function.Unicode ( (∘) )

import Text.PrettyPrint
import Text.Printf ( printf )

import Test.Complexity.Base ( MeasurementStats(..)
                            , Sample
                            , Stats(..)
                            )

prettyStats ∷ MeasurementStats → Doc
prettyStats (MeasurementStats {..}) =   text "desc:" <+> text msDesc
                                    $+$ text ""
                                    $+$ vcat (map ppSample msSamples)
    where ppSample ∷ Sample → Doc
          ppSample (x, y) = (text ∘ printf "%3i") x <+> char '|' <+> ppStats y
          ppStats (Stats {..}) = int (length statsSamples)
                                 <+> hsep (map (text ∘ printf "%7.3f")
                                               [statsMin, statsMean2, statsMax, statsStdDev]
                                          )

printStats ∷ [MeasurementStats] → IO ()
printStats = mapM_ (\s → do putStrLn ∘ render ∘ prettyStats $ s
                            putStrLn ""
                   )
