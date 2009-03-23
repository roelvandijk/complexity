{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns  #-}

module Test.Complexity.Chart where

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk
import Data.Accessor

import Data.Colour
import qualified Data.Colour.Names as CN
import Data.Colour.SRGB

import Test.Complexity.Types
import Test.Complexity.Statistics


colourToCairo :: Colour Double -> Color
colourToCairo c = let rgb = toSRGB c
                  in Color { c_r = channelRed   rgb
                           , c_g = channelGreen rgb
                           , c_b = channelBlue  rgb
                           }


statsToChart :: [(EvalStats, Colour Double)] -> Layout1 Double Double
statsToChart [] = defaultLayout1
statsToChart xs = layout1_title ^= "test with multiple eval stats"
                $ layout1_plots ^= concat [map Left $ statsToPlots colour (timeStats stats) | (stats, colour) <- xs]
                $ layout1_left_axis   .> laxis_title ^= "time (ms)"
                $ layout1_bottom_axis .> laxis_title ^= "input size (n)"
                $ defaultLayout1

statsToPlots :: Num n => Colour Double -> [SampleStats] -> [Plot n Double]
statsToPlots c timeStats = [ toPlot cpuMinMax
                           , toPlot cpuMin
                           , toPlot cpuMax
                           , toPlot cpuMean
                           , toPlot cpuErr
                           , toPlot cpuMeanPts
                           ]
    where colour1 = colourToCairo c
          colour2 = colourToCairo $ blend 0.3 c CN.white
          colour3 = colourToCairo $ blend 0.5 c CN.white
          colour4 = colourToCairo $ blend 0.7 c CN.white
          colour5 = colourToCairo $ blend 0.5 c CN.black

          cpuMean = plot_lines_values ^= [zip xs ys_cpuMean]
                  $ plot_lines_style  .> line_color ^= colour1
                  $ defaultPlotLines

          cpuMin = plot_lines_values ^= [zip xs ys_cpuMin]
                 $ plot_lines_style .> line_color ^= colour3
                 $ defaultPlotLines

          cpuMax = plot_lines_values ^= [zip xs ys_cpuMax]
                 $ plot_lines_style .> line_color ^= colour3
                 $ defaultPlotLines

          cpuMeanPts = plot_points_values ^= zip xs ys_cpuMean
                     $ plot_points_style  ^= filledCircles 2 colour5
                     $ defaultPlotPoints

          cpuMinMax = plot_fillbetween_values ^= zip xs (zip ys_cpuMin ys_cpuMax)
                    $ plot_fillbetween_style  ^= solidFillStyle colour2
                    $ defaultPlotFillBetween

          cpuErr = plot_errbars_values ^= [symErrPoint x y 0 e | (x, y, e) <- zip3 xs ys_cpuMean vs_cpuStdDev]
                 $ plot_errbars_line_style  .> line_color ^= colour4
                 $ defaultPlotErrBars

          xs           = [fromIntegral inputSize | (SampleStats {inputSize}) <- timeStats]
          ys_cpuMean   = [stMean   cpuTime | (SampleStats {cpuTime}) <- timeStats]
          ys_cpuMin    = [stMin    cpuTime | (SampleStats {cpuTime}) <- timeStats]
          ys_cpuMax    = [stMax    cpuTime | (SampleStats {cpuTime}) <- timeStats]
          vs_cpuStdDev = [stStdDev cpuTime | (SampleStats {cpuTime}) <- timeStats]

quickToChart :: [EvalStats] -> IO ()
quickToChart xs = renderableToWindow (toRenderable $ statsToChart $ zip xs colours) 640 480
    where colours = [ CN.blue
                    , CN.red
                    , CN.green
                    , CN.goldenrod
                    , CN.cyan
                    , CN.wheat
                    ] ++ repeat CN.silver
