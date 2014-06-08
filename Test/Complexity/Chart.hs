{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE PackageImports  #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Complexity.Chart ( statsToChart
                             , quickStatsToChart
                             , showStatsChart
                             ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Data.List ( intercalate )
import "Chart" Graphics.Rendering.Chart
import "Chart-gtk" Graphics.Rendering.Chart.Gtk
import "colour" Data.Colour
import "colour" Data.Colour.Names
import "this" Test.Complexity.Types
import "data-default-class" Data.Default.Class ( def )
import "lens" Control.Lens

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


statsToChart :: [(MeasurementStats, Colour Double)] -> Layout Double Double
statsToChart [] = def
statsToChart xs = layout_title .~ intercalate ", " [msDesc | (MeasurementStats {msDesc}, _) <- xs]
                $ layout_plots .~ concat [statsToPlots colour stats | (stats, colour) <- xs]
                $ layout_y_axis . laxis_title .~ "time (ms)"
                $ layout_x_axis . laxis_title .~ "input size (n)"
                $ def

quickStatsToChart :: [MeasurementStats] -> Layout Double Double
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
statsToPlots c stats = [ plot_legend .~ [] $ toPlot cpuMinMax
                       , plot_legend .~ [] $ toPlot cpuMin
                       , plot_legend .~ [] $ toPlot cpuMax
                       , toPlot cpuMean
                       , plot_legend .~ [] $ toPlot cpuMeanPts
                       ]
    where meanLineColour   = opaque c
          meanPointColour  = opaque $ blend 0.5 c black
          minMaxEdgeColour = withOpacity c 0.2
          minMaxAreaColour = withOpacity c 0.05

          cpuMean = plot_lines_values .~ [zip xs ys_cpuMean]
                  $ plot_lines_style  . line_color .~ meanLineColour
                  $ plot_lines_title .~ msDesc stats
                  $ def

          cpuMin = plot_lines_values .~ [zip xs ys_cpuMin]
                 $ plot_lines_style . line_color .~ minMaxEdgeColour
                 $ def

          cpuMax = plot_lines_values .~ [zip xs ys_cpuMax]
                 $ plot_lines_style . line_color .~ minMaxEdgeColour
                 $ def

          cpuMeanPts = plot_points_values .~ zip xs ys_cpuMean
                     $ plot_points_style  .~ filledCircles 2 meanPointColour
                     $ def

          cpuMinMax = plot_fillbetween_values .~ zip xs (zip ys_cpuMin ys_cpuMax)
                    $ plot_fillbetween_style  .~ solidFillStyle minMaxAreaColour
                    $ def


          ps         = msSamples stats
          xs         = map (fromIntegral . fst) ps
          ys_cpuMean = map (stMean . snd) ps
          ys_cpuMin  = map (stMin  . snd) ps
          ys_cpuMax  = map (stMax  . snd) ps

showStatsChart :: [MeasurementStats] -> IO ()
showStatsChart xs = renderableToWindow (toRenderable $ quickStatsToChart xs) 640 480
