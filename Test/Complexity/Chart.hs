{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns  #-}

module Test.Complexity.Chart ( statsToChart
                             , quickStatsToChart
                             , showStatsChart
                             ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Data.List ( intercalate )

-- from Chart:
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk

-- from colour:
import Data.Colour
import Data.Colour.Names

-- from complexity:
import Test.Complexity.Base ( MeasurementStats(..)
                            , Stats(..)
                            )


-- from data-accessor:
import Data.Accessor

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


statsToChart :: [(MeasurementStats, Colour Double)] -> Layout1 Double Double
statsToChart [] = defaultLayout1
statsToChart xs = layout1_title ^= intercalate ", " [msDesc | (MeasurementStats {msDesc}, _) <- xs]
                $ layout1_plots ^= concat [map Left $ statsToPlots colour stats | (stats, colour) <- xs]
                $ layout1_left_axis   .> laxis_title ^= "time (ms)"
                $ layout1_bottom_axis .> laxis_title ^= "input size (n)"
                $ defaultLayout1

quickStatsToChart :: [MeasurementStats] -> Layout1 Double Double
quickStatsToChart xs = statsToChart $ zip xs $ cycle colours
    where colours = [ blue
                    , red
                    , green
                    , darkgoldenrod
                    , orchid
                    , sienna
                    , darkcyan
                    , olivedrab
                    , silver
                    ]

statsToPlots :: Colour Double -> MeasurementStats -> [Plot Double Double]
statsToPlots c stats = [ plot_legend ^= [] $ toPlot cpuMinMax
                       , plot_legend ^= [] $ toPlot cpuMin
                       , plot_legend ^= [] $ toPlot cpuMax
                       , toPlot cpuMean
                       , plot_legend ^= [] $ toPlot cpuErr
                       , plot_legend ^= [] $ toPlot cpuMeanPts
                       ]
    where meanLineColour   = opaque c
          meanPointColour  = opaque $ blend 0.5 c black
          stdDevColour     = withOpacity c 0.7
          minMaxEdgeColour = withOpacity c 0.2
          minMaxAreaColour = withOpacity c 0.05

          cpuMean = plot_lines_values ^= [zip xs ys_cpuMean2]
                  $ plot_lines_style  .> line_color ^= meanLineColour
                  $ plot_lines_title ^= msDesc stats
                  $ defaultPlotLines

          cpuMin = plot_lines_values ^= [zip xs ys_cpuMin]
                 $ plot_lines_style .> line_color ^= minMaxEdgeColour
                 $ defaultPlotLines

          cpuMax = plot_lines_values ^= [zip xs ys_cpuMax]
                 $ plot_lines_style .> line_color ^= minMaxEdgeColour
                 $ defaultPlotLines

          cpuMeanPts = plot_points_values ^= zip xs ys_cpuMean2
                     $ plot_points_style  ^= filledCircles 2 meanPointColour
                     $ defaultPlotPoints

          cpuMinMax = plot_fillbetween_values ^= zip xs (zip ys_cpuMin ys_cpuMax)
                    $ plot_fillbetween_style  ^= solidFillStyle minMaxAreaColour
                    $ defaultPlotFillBetween

          cpuErr = plot_errbars_values ^= [symErrPoint x y 0 e | (x, y, e) <- zip3 xs ys_cpuMean vs_cpuStdDev]
                 $ plot_errbars_line_style  .> line_color ^= stdDevColour
                 $ defaultPlotErrBars

          ps      = msSamples stats
          xs           = map (fromIntegral . fst) ps
          ys_cpuMean   = map (statsMean    . snd) ps
          ys_cpuMean2  = map (statsMean2   . snd) ps
          ys_cpuMin    = map (statsMin     . snd) ps
          ys_cpuMax    = map (statsMax     . snd) ps
          vs_cpuStdDev = map (statsStdDev  . snd) ps

showStatsChart :: [MeasurementStats] -> IO ()
showStatsChart xs = renderableToWindow (toRenderable $ quickStatsToChart xs) 640 480
