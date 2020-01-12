module Main where

import           VoiceLeading.Base             as VL
import           VoiceLeading.Theory
import           VoiceLeading.Automaton
import           VoiceLeading.Distribution
import           VoiceLeading.Learning
import           VoiceLeading.Inference

import           VoiceLeading.IO.Model
import           VoiceLeading.IO.Midi
import           VoiceLeading.IO.LilyPond
import           VoiceLeading.IO.Plotting
import qualified Data.Map                      as M

import           Control.Monad
import           Data.Default
import qualified Data.Vector                   as V
import qualified Data.Vector.Unboxed           as VU
import           Data.Function.Memoize
import           Data.Maybe                     ( catMaybes )

-- pitchify i = Pitch i False
-- joinVoices a b = toEv $ M.fromList [(CF, a), (LowCP, b)]

-- cantus = map pitchify [57, 60, 62, 59, 60, 65, 62, 64, 60, 59, 57]
-- fstCP = Pitch 57 True : map pitchify [57, 53, 55, 48, 50, 53, 52, 57, 56, 57]

-- cp1 = zipWith joinVoices cantus fstCP

main1 :: IO ()
main1 = do
  model <- loadModel "model.json"
  let mfNames = nfName <$> modelFeatures model
  plottingLogger "diagram.pdf" model
  -- (est,score) <- bestEstimate (replicateM 10 $ mapEstimateNotewise Nothing model 11 []) model
  piece <- testPiece
  let aopts = def
  est <- estimateGibbsNotes aopts [Soprano, Bass] piece model 20 (const 5.0)
  putStrLn $ "logpot/event estimate: " ++ show
    (meanLogPotential aopts est model)
  let avgFEst = meanFeatCounts aopts est model
  plotOverFeatures "estimate_feats.pdf"
                   "Mean Feature Values (MAP Estimate)"
                   mfNames
                   avgFEst
  ps <- corpusPieces
  putStrLn $ "logpot/event corpus: " ++ show (meanLogPotentialN aopts ps model)
  let avgFCorp = meanFeatCountsN aopts ps model
  plotOverFeatures "corpus_feats.pdf"
                   "Mean Feature Values (Corpus)"
                   mfNames
                   avgFCorp
  let avgFDiff = VU.map log $ VU.zipWith (/) avgFEst avgFCorp
  printOverFeatures "relative_feats.pdf"
                    "Mean Feature Values (Estimate/Corpus)"
                    mfNames
                    avgFDiff
  plotOverFeatures "relative_feats.pdf"
                   "Mean Feature Values (Estimate/Corpus)"
                   mfNames
                   avgFDiff
  viewPieceTmp est

main2 = do
  pieces <- corpusPieces
  let splitn        = round $ fromIntegral (length pieces) * 0.7
      (train, test) = splitAt splitn pieces
      feats         = V.fromList $ nfFeature <$> defaultFeaturesNamed
  putStrLn $ show $ expectedFeats def train feats
  putStrLn $ show $ expectedFeats def test feats

main = do
  (Just pfs) <- loadProfiles "data/jsbach_chorals_harmony/profiles.json"
  let harm    = traceMemoize $ matchChordProfiles $ vectorizeProfiles pfs
      pitches = (Just <$> [0 .. 11]) <> [Nothing]
  putStrLn $ show $ sum $ do
    p1 <- pitches
    p2 <- pitches
    p3 <- pitches
    p4 <- pitches
    pure $ snd $ harm $ catMaybes [p1, p2, p3, p4]
  putStrLn $ show $ sum $ do
    p1 <- reverse pitches
    p2 <- reverse pitches
    p3 <- reverse pitches
    p4 <- reverse pitches
    pure $ snd $ harm $ catMaybes [p1, p2, p3, p4]
