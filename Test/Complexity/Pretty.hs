{-# LANGUAGE RecordWildCards #-}

module Test.Complexity.Pretty ( prettyStats
                              , printStats
                              ) where

import Text.PrettyPrint
import Text.Printf (printf)

import Test.Complexity ( SampleStats(..)
                       , MeasurementStats(..)
                       , Stats(..)
                       )

prettyStats :: MeasurementStats -> Doc
prettyStats (MeasurementStats {..}) =   text "desc:" <+> text desc
                                    $+$ text ""
                                    $+$ vcat (map ppSample timeStats)
    where ppSample :: SampleStats -> Doc
          ppSample (SampleStats {..}) = (text . printf "%3i") inputSize <+> char '|'
                                        <+> text "cpu"  <+> ppStats cpuTime
                                        <+> char '|'
                                        <+> text "wall" <+> ppStats wallTime
          ppStats (Stats {..}) = int statsSamples
                                 <+> hsep (map (text . printf "%7.3f")
                                               [statsMin, statsMean2, statsMax, statsStdDev]
                                          )

printStats :: [MeasurementStats] -> IO ()
printStats = mapM_ (\s -> do putStrLn . render . prettyStats $ s
                             putStrLn ""
                   )
