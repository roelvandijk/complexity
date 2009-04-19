{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns  #-}

module Test.Complexity.Chart ( statsToChart
                             , quickStatsToChart
                             , showStatsChart
                             ) where

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk
import Data.Accessor
import Data.List (intercalate)

import Data.Colour
import qualified Data.Colour.Names as CN
import Data.Colour.SRGB

import Test.Complexity.Base ( MeasurementStats(..)
                            , Stats(..)
                            )


colourToCairo :: Colour Double -> Color
colourToCairo c = let rgb = toSRGB c
                  in Color { c_r = channelRed   rgb
                           , c_g = channelGreen rgb
                           , c_b = channelBlue  rgb
                           }

statsToChart :: [(MeasurementStats, Colour Double)] -> Layout1 Double Double
statsToChart [] = defaultLayout1
statsToChart xs = layout1_title ^= intercalate ", " [msDesc | (MeasurementStats {msDesc}, _) <- xs]
                $ layout1_plots ^= concat [map Left $ statsToPlots colour stats | (stats, colour) <- xs]
                $ layout1_left_axis   .> laxis_title ^= "time (ms)"
                $ layout1_bottom_axis .> laxis_title ^= "input size (n)"
                $ defaultLayout1

quickStatsToChart :: [MeasurementStats] -> Layout1 Double Double
quickStatsToChart xs = statsToChart $ zip xs $ cycle colours
    where colours = [ CN.blue
                    , CN.red
                    , CN.green
                    , CN.darkgoldenrod
                    , CN.orchid
                    , CN.sienna
                    , CN.darkcyan
                    , CN.olivedrab
                    , CN.silver
                    ]

statsToPlots :: Colour Double -> MeasurementStats -> [Plot Double Double]
statsToPlots c stats = --[ plot_legend ^= [] $ toPlot cpuMinMax
                       --, plot_legend ^= [] $ toPlot cpuMin
                       --, plot_legend ^= [] $ toPlot cpuMax
                       --] ++
                       [ toPlot cpuMean
                       , plot_legend ^= [] $ toPlot cpuErr
                       , plot_legend ^= [] $ toPlot cpuMeanPts
                       ]
    where colour_normal   = colourToCairo c
          colour_dark     = colourToCairo $ blend 0.5 c CN.black
          colour_light    = colourToCairo $ blend 0.7 c CN.white
--           colour_lighter  = colourToCairo $ blend 0.5 c CN.white
--           colour_lightest = colourToCairo $ blend 0.3 c CN.white

          cpuMean = plot_lines_values ^= [zip xs ys_cpuMean2]
                  $ plot_lines_style  .> line_color ^= colour_normal
                  $ plot_lines_title ^= msDesc stats
                  $ defaultPlotLines

--           cpuMin = plot_lines_values ^= [zip xs ys_cpuMin]
--                  $ plot_lines_style .> line_color ^= colour_lighter
--                  $ defaultPlotLines

--           cpuMax = plot_lines_values ^= [zip xs ys_cpuMax]
--                  $ plot_lines_style .> line_color ^= colour_lighter
--                  $ defaultPlotLines

          cpuMeanPts = plot_points_values ^= zip xs ys_cpuMean2
                     $ plot_points_style  ^= filledCircles 2 colour_dark
                     $ defaultPlotPoints

--           cpuMinMax = plot_fillbetween_values ^= zip xs (zip ys_cpuMin ys_cpuMax)
--                     $ plot_fillbetween_style  ^= solidFillStyle colour_lightest
--                     $ defaultPlotFillBetween

          cpuErr = plot_errbars_values ^= [symErrPoint x y 0 e | (x, y, e) <- zip3 xs ys_cpuMean vs_cpuStdDev]
                 $ plot_errbars_line_style  .> line_color ^= colour_light
                 $ defaultPlotErrBars

          ps      = msSamples stats
          xs           = map (fromIntegral . fst) ps
          ys_cpuMean   = map (statsMean    . snd) ps
          ys_cpuMean2  = map (statsMean2   . snd) ps
--        ys_cpuMin    = map (statsMin     . snd) ps
--        ys_cpuMax    = map (statsMax     . snd) ps
          vs_cpuStdDev = map (statsStdDev  . snd) ps

showStatsChart :: [MeasurementStats] -> IO ()
showStatsChart xs = renderableToWindow (toRenderable $ quickStatsToChart xs) 640 480
