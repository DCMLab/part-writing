module VoiceLeading.IO.Plotting where

import VoiceLeading.Distribution
import VoiceLeading.Automaton (NamedFeature(..))

import Control.Concurrent (forkIO)
-- import Data.Map.Strict as M

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

-- deriving instance (Generic a, Generic b) => Generic (M.Map a b)

plottingSetup :: IO ()
plottingSetup = pure ()

plottingLogger :: Show v => Model v -> IO ()
plottingLogger model = do
  putStrLn "log"
  toFile def "diagram.png" $ do
    layout_title .= "Model feature weights"
    layout_x_axis . laxis_generate .= autoIndexAxis (map nfName nfs)
    plot $ fmap plotBars $ bars ["parameter value"] (addIndexes (map (:[]) ps))
      where (Model nfs ps) = model
            mkParamMap = zipWith (\n p -> (n,[p])) (map nfName nfs) ps

