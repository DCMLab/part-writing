module Main where

import VoiceLeading.Base as VL
import VoiceLeading.Theory
import VoiceLeading.IO.Midi
import VoiceLeading.IO.LilyPond
import VoiceLeading.Automaton
import VoiceLeading.Distribution
import VoiceLeading.Learning
import VoiceLeading.Inference
import VoiceLeading.IO.Plotting
import qualified Data.Map as M

import Control.Monad

-- pitchify i = Pitch i False
-- joinVoices a b = toEv $ M.fromList [(CF, a), (LowCP, b)]

-- cantus = map pitchify [57, 60, 62, 59, 60, 65, 62, 64, 60, 59, 57]
-- fstCP = Pitch 57 True : map pitchify [57, 53, 55, 48, 50, 53, 52, 57, 56, 57]

-- cp1 = zipWith joinVoices cantus fstCP

main :: IO ()
main = do
  model <- loadModel "model.json"
  let mfNames = map nfName (modelFeatures model)
  plottingLogger "diagram.pdf" model
  -- (est,score) <- bestEstimate (replicateM 10 $ mapEstimateNotewise Nothing model 11 []) model
  piece <- testPiece
  est <- estimateGibbsAnnealing piece model [Soprano, Bass] 20 (const 5.0)
  putStrLn $ "logpot/event estimate: " ++ show (meanLogPotential est model)
  let avgFEst = meanFeatCounts est model
  plotOverFeatures "estimate_feats.pdf"
    "Mean Feature Values (MAP Estimate)"
    mfNames avgFEst
  ps <- corpusPieces
  putStrLn $ "logpot/event corpus: " ++ show (meanLogPotentialN ps model)
  let avgFCorp = meanFeatCountsN ps model
  plotOverFeatures "corpus_feats.pdf"
    "Mean Feature Values (Corpus)"
    mfNames avgFCorp
  let avgFDiff = map log $ zipWith (/) avgFEst avgFCorp
  printOverFeatures "relative_feats.pdf"
    "Mean Feature Values (Estimate/Corpus)"
    mfNames avgFDiff
  plotOverFeatures "relative_feats.pdf"
    "Mean Feature Values (Estimate/Corpus)"
    mfNames avgFDiff
  viewPieceTmp est
