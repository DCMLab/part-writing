module VoiceLeading.Theory where

import VoiceLeading.Base
import VoiceLeading.Helpers (rotate)

import Data.List (elemIndex)
import Data.Maybe (mapMaybe)
import Data.Ratio
import Data.Function.Memoize (memoize)

import qualified Debug.Trace as DT

modal i m = [0, 2, 4, 5, 7, 9, 11] !! (mod (i+m) 7)

cMajor :: [Int]
cMajor = [2,2,1,2,2,2,1]

scale :: KeySig -> [Int]
scale = memoize s
  where s (KeySig root mode) = scanl (\a b -> (a+b) `mod` 12) root (init $ rotate mode cMajor)

leadingTone :: KeySig -> Int
leadingTone = memoize lt
  where lt (KeySig root _) = (root - 1) `mod` 12

majorChord :: [Int]
majorChord = [0, 4, 7, 10, 11, 2, 1, 5, 6, 9, 8, 3]

minorChord :: [Int]
minorChord = [0, 3, 7, 10, 2, 11, 1, 5, 6, 8, 9, 4]

findRoot :: [Int] -> (Int, Double)
findRoot []      = (0, 0)
findRoot pitches = testRoot [0..11]
  where ps = map (`mod` 12) pitches
        indexVal :: Int -> Rational
        indexVal i = 1%(fromIntegral i + 1)
        testChord chord tps = sum $ map val [0 .. maximum is]
          where is = mapMaybe (flip elemIndex chord) tps
                val i = if i `elem` is then indexVal i else 0 - indexVal i
        testRoot (r:rs) = let tps   = map ((`mod` 12) . (\x -> x-r)) ps
                              score = max (testChord majorChord tps) (testChord minorChord tps)
                          in testRoot' rs r score
        testRoot' [] best score = (best, fromRational score)
        testRoot' (r:rs) best score = if newScore > score
                                      then testRoot' rs r newScore
                                      else testRoot' rs best score
          where tps = map ((`mod` 12) . (\x -> x-r)) ps
                newScore = max (testChord majorChord tps) (testChord minorChord tps)
