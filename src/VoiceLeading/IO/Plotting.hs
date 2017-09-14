module VoiceLeading.IO.Plotting where

import VoiceLeading.Distribution
import VoiceLeading.Automaton (NamedFeature(..))

import Control.Concurrent (forkIO)
import Control.Monad (zipWithM_)
import qualified Data.Text as T

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo (toFile, FileOptions(..), FileFormat(..))

import VoiceLeading.IO.HorizontalBars

plotOverFeatures :: FilePath -> String -> [T.Text] -> [Double] -> IO ()
plotOverFeatures fp title names values = do
    -- putStrLn "log"
  toFile opts fp $ do
    layout_title .= title
    layout_top_axis_visibility .= def
    layout_y_axis . laxis_generate .= autoIndexAxis (map T.unpack names)
    layout_y_axis . laxis_style . axis_label_style . font_size .= 8
    plot $ plotHBars <$> barPlot -- bars ["parameter value"] (addIndexes (map (:[]) ps))
      where --mkParamMap = zipWith (\n p -> (n,[p])) (map nfName nfs) ps
            opts = FileOptions (600,2100) PDF
            barPlot = liftEC $ do
              color <- takeColor
              let styles = [(solidFillStyle color, Just (solidLine 0.5 $ opaque black))]
              plot_bars_item_styles .= styles
              plot_bars_titles .= ["parameter value"]
              plot_bars_values .= (addIndexes (map (:[]) values))
              plot_bars_style .= BarsClustered
              plot_bars_spacing .= BarsFixGap 30 5

printOverFeatures :: FilePath -> String -> [T.Text] -> [Double] -> IO ()
printOverFeatures _ title names values = do
  putStrLn title
  zipWithM_ printPair names values
  putStrLn ""
  where printPair n v = putStrLn $ show n ++ ": " ++ show v

plottingSetup :: IO ()
plottingSetup = pure ()

plottingLogger :: Show v => FilePath -> Model v -> IO ()
plottingLogger fp model = plotOverFeatures fp "Model feature weights" names vals
  where (Model nfs vals) = model
        names = map nfName nfs
