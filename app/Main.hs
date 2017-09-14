module Main where

import VoiceLeading.Base as VL
import VoiceLeading.Theory
import VoiceLeading.IO.Midi
import VoiceLeading.IO.LilyPond
import VoiceLeading.Automaton
import VoiceLeading.Distribution
import VoiceLeading.IO.Plotting
import qualified Data.Map as M

-- pitchify i = Pitch i False
-- joinVoices a b = toEv $ M.fromList [(CF, a), (LowCP, b)]

-- cantus = map pitchify [57, 60, 62, 59, 60, 65, 62, 64, 60, 59, 57]
-- fstCP = Pitch 57 True : map pitchify [57, 53, 55, 48, 50, 53, 52, 57, 56, 57]

-- cp1 = zipWith joinVoices cantus fstCP

main :: IO ()
main = do
  -- load a piece from Midi
  p <- loadMidi "01AusmeinesHerz.mid" :: IO (Piece ChoralVoice)
  -- give it a nicer title
  let (Piece meta events) = p
      piece = Piece (meta { title = "Aus meines Herzens Grunde" }) events
  -- do something
  ps <- corpusPieces
  plotInfo <- plottingSetup
  --newPiece <- gibbsNotePiece1 piece
  --pure ()
  --viewPiece newPiece -- shows the internal representation of a piece as notes
  --newPiece <- gibbsEv1 piece
  --print newPiece
  
  
  -- pieces <- loadPieces "chorales.json" :: IO (Pieces ChoralVoice)
  -- p <- return $ head pieces
  -- -- print $ take 10 (analysePiece (toEPiece p) :: [VLState])
  -- print (analysePiece stateAuto (toEPiece cp1))


  --putStrLn  (show (learnAll1 pieces :: ProductOfExperts))
