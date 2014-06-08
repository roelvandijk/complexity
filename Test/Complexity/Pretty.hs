{-# LANGUAGE PackageImports  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax   #-}

module Test.Complexity.Pretty (prettyStats, printStats) where

import "base-unicode-symbols" Data.Function.Unicode ( (∘) )
import qualified "vector" Data.Vector as V
import "this" Test.Complexity.Types ( MeasurementStats(..), Sample )
import "pretty" Text.PrettyPrint
import "base" Text.Printf ( printf )


prettyStats ∷ MeasurementStats → Doc
prettyStats (MeasurementStats {..}) =   text "desc:" <+> text msDesc
                                    $+$ text ""
                                    $+$ vcat (map ppSample msSamples)
    where ppSample ∷ Sample → Doc
          ppSample (x, y) = (text ∘ printf "%3i") x <+> char '|' <+> ppStats y
          ppStats (Stats {..}) = int (V.length statsSamples)
                                 <+> hsep (map (text ∘ printf "%13.9f")
                                               [statsMin, statsMean2, statsMax, statsStdDev]
                                          )

printStats ∷ [MeasurementStats] → IO ()
printStats = mapM_ (\s → do putStrLn ∘ render ∘ prettyStats $ s
                            putStrLn ""
                   )
