module Main where

import VoiceLeading.Base as VL hiding ((!), (!?))
import VoiceLeading.Load as Load
import VoiceLeading.Prob as Prob

main :: IO ()
main = do
  pieces <- Load.loadPieces "chorales.json"
  putStrLn  (show (learnAll1 pieces :: ProductOfExperts))
