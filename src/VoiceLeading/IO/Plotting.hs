module VoiceLeading.IO.Plotting where

import           VoiceLeading.Distribution
import           VoiceLeading.Features          ( NamedFeature(..) )

import qualified Data.Text                     as T
import           Control.Lens                   ( use )
import qualified Data.Vector                   as V
import qualified Data.Vector.Unboxed           as VU

import           Graphics.Rendering.Chart.Easy
import           Graphics.Rendering.Chart.Backend.Cairo
                                                ( toFile
                                                , FileOptions(..)
                                                , FileFormat(..)
                                                )
import qualified Data.Colour.Palette.ColorSet  as K
-- import qualified Data.Colour.Palette.BrewerSet as K

import           VoiceLeading.IO.HorizontalBars

plotOverFeatures
  :: FilePath -> String -> V.Vector T.Text -> VU.Vector Double -> IO ()
plotOverFeatures fp title names vals = toFile opts fp $ do
  layout_title .= title
  layout_top_axis_visibility .= def
  layout_y_axis . laxis_generate .= autoIndexAxis labels
  layout_y_axis . laxis_style . axis_label_style . font_size .= 8
  setColors $ opaque . K.d3Colors1 <$> [0 ..]
  plot $ plotHBars <$> barPlot
 where
  opts    = FileOptions (600, 2200) PDF
  labels  = reverse $ T.unpack <$> V.toList names
  values  = reverse $ (: []) <$> VU.toList vals
  barPlot = liftEC $ do
    cols <- liftCState $ use colors
    plot_bars_item_styles .= [ (solidFillStyle c, Nothing) | c <- cols ]
    plot_bars_titles .= ["parameter value"]
    plot_bars_values .= addIndexes values
    plot_bars_style .= BarsClustered
    plot_bars_spacing .= BarsFixGap 30 5

printOverFeatures
  :: FilePath -> String -> V.Vector T.Text -> VU.Vector Double -> IO ()
printOverFeatures _ title names values = do
  putStrLn title
  V.zipWithM_ printPair names $ V.convert values
  putStrLn ""
  where printPair n v = putStrLn $ show n ++ ": " ++ show v

plottingLogger :: FilePath -> Model v -> IO ()
plottingLogger fp model = plotOverFeatures fp
                                           "Model feature weights"
                                           names
                                           vals
 where
  (Model nfs vals) = model
  names            = nfName <$> nfs

