module VoiceLeading.IO.Plotting where

import VoiceLeading.Distribution
import VoiceLeading.Automaton (NamedFeature(..))

import Control.Concurrent (forkIO)
import Control.Monad (zipWithM_)
import qualified Data.Text as T
import Control.Lens (use)

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo (toFile, FileOptions(..), FileFormat(..))
import qualified Data.Colour.Palette.ColorSet as K
import qualified Data.Colour.Palette.BrewerSet as K

import VoiceLeading.IO.HorizontalBars

plotOverFeatures :: FilePath -> String -> [T.Text] -> [Double] -> IO ()
plotOverFeatures fp title names values = do
    -- putStrLn "log"
  toFile opts fp $ do
    layout_title .= title
    layout_top_axis_visibility .= def
    layout_y_axis . laxis_generate .= autoIndexAxis (reverse $ map T.unpack names)
    layout_y_axis . laxis_style . axis_label_style . font_size .= 8
    setColors $ opaque . K.d3Colors1 <$> [0..]
    plot $ plotHBars <$> barPlot -- bars ["parameter value"] (addIndexes (map (:[]) ps))
      where --mkParamMap = zipWith (\n p -> (n,[p])) (map nfName nfs) ps
            opts = FileOptions (600,2200) PDF
            barPlot = liftEC $ do
              cols <- liftCState $ use colors
              plot_bars_item_styles .= [(solidFillStyle c, Nothing) | c <- cols]
              plot_bars_titles .= ["parameter value"]
              plot_bars_values .= (addIndexes (reverse $ map (:[]) values))
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
