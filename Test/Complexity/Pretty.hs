{-# LANGUAGE RecordWildCards #-}

module Test.Complexity.Pretty where

import Text.PrettyPrint
import Text.Printf (printf)

import Test.Complexity.Types
import Test.Complexity.Statistics

ppEvalStats :: EvalStats -> Doc
ppEvalStats (EvalStats {..}) = text "desc:" <+> text desc
                               $+$ text ""
                               $+$ vcat (map ppSample timeStats)
    where ppSample :: SampleStats -> Doc
          ppSample (SampleStats {..}) = (text . printf "%3i") inputSize <+> char '|'
                                        <+> text "cpu"  <+> ppStats cpuTime
                                        <+> char '|'
                                        <+> text "wall" <+> ppStats wallTime
          ppStats (Stats {..}) = int stSamples
                                 <+> hsep (map (text . printf "%7.3f")
                                               [stMin, stMean, stMedian, stMax, stStdDev]
                                          )

quickPrint :: [EvalStats] -> IO ()
quickPrint = mapM_ (\s -> do putStrLn . render . ppEvalStats $ s
                             putStrLn ""
                   )
