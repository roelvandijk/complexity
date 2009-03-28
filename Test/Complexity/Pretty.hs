{-# LANGUAGE RecordWildCards #-}

module Test.Complexity.Pretty where

import Text.PrettyPrint
import Text.Printf (printf)

import Test.Complexity ( SampleStats(..)
                       , EvalStats(..)
                       , Stats(..)
                       )


ppEvalStats :: EvalStats -> Doc
ppEvalStats (EvalStats {..}) = text "desc:" <+> text desc
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

quickPrint :: [EvalStats] -> IO ()
quickPrint = mapM_ (\s -> do putStrLn . render . ppEvalStats $ s
                             putStrLn ""
                   )
