module VoiceLeading.Theory where

import           VoiceLeading.Base
import           VoiceLeading.Helpers           ( rotate )

import           Data.Foldable                  ( foldl' )
import           Data.List                      ( elemIndex
                                                , maximumBy
                                                )
import           Data.Maybe                     ( mapMaybe )
import           Data.Ratio
import           Data.Function.Memoize          ( memoize )

import           Data.Aeson
import qualified Data.Map                      as M
import qualified Data.Vector                   as V
import qualified Data.Vector.Unboxed           as VU
import qualified Data.Vector.Unboxed.Mutable   as VUM

import qualified Debug.Trace                   as DT
import           Numeric.SpecFunctions          ( logBeta )
import qualified Control.Loop                  as L

modal i m = [0, 2, 4, 5, 7, 9, 11] !! mod (i + m) 7

cMajor :: [Int]
cMajor = [2, 2, 1, 2, 2, 2, 1]

scale :: KeySig -> [Int]
scale = memoize s
 where
  s (KeySig root mode) =
    scanl (\a b -> (a + b) `mod` 12) root (init $ rotate mode cMajor)

leadingTone :: KeySig -> Int
leadingTone = memoize lt where lt (KeySig root _) = (root - 1) `mod` 12

majorChord :: [Int]
majorChord = [0, 4, 7, 10, 11, 2, 1, 5, 6, 9, 8, 3]

minorChord :: [Int]
minorChord = [0, 3, 7, 10, 2, 11, 1, 5, 6, 8, 9, 4]

-- | Returns a pair of the estimated root and the "chordness" of the given set of pitches
findHarm :: [Int] -> (Int, Double)
findHarm []      = (0, 0)
findHarm pitches = testRoot [0 .. 11]
 where
  ps = map (`mod` 12) pitches
  indexVal :: Int -> Rational
  indexVal i = 1 % (fromIntegral i + 1)
  testChord chord tps = sum $ map val [0 .. maximum is]
   where
    is = mapMaybe (flip elemIndex chord) tps
    val i = if i `elem` is then indexVal i else negate (indexVal i)
  testRoot (r : rs) =
    let tps   = map ((`mod` 12) . (\x -> x - r)) ps
        score = max (testChord majorChord tps) (testChord minorChord tps)
    in  testRoot' rs r score
  testRoot' []       best score = (best, fromRational score)
  testRoot' (r : rs) best score = if newScore > score
    then testRoot' rs r newScore
    else testRoot' rs best score
   where
    tps      = map ((`mod` 12) . (\x -> x - r)) ps
    newScore = max (testChord majorChord tps) (testChord minorChord tps)

type Profile = (Double, VU.Vector Double)
type Profiles = V.Vector Profile

loadProfiles :: FilePath -> IO (Maybe (M.Map String (M.Map Int Double)))
loadProfiles = decodeFileStrict'

-- | Returns a vector of profiles.
-- Each profile is a pair of the total counts in the profile and the smoothed count vector.
vectorizeProfiles :: M.Map String (M.Map Int Double) -> Profiles
vectorizeProfiles profmap = V.fromList $ profList <$> M.elems profmap
 where
  profList pm = (VU.sum lst, lst)
    where lst = VU.fromList [ 1 + pm M.! i | i <- [0 .. 11] ]

logDirichletMultinomial :: Profile -> VU.Vector Double -> Double -> Double
logDirichletMultinomial (a0, as) xs n = num - denom
 where
  l = VU.length xs
  entry acc i = if xi > 0 then acc + log xi + logBeta (as VU.! i) xi else acc
    where xi = (xs VU.! i)
  num   = log n + logBeta a0 n
  denom = L.forLoopFold 0 (< l) (+ 1) 0 entry

mkChord :: Int -> [Int] -> VU.Vector Double
mkChord r pitches = VU.create $ do
  chord <- VUM.replicate 12 0
  mapM_ (VUM.unsafeModify chord (+ 1) . (`mod` 12) . (subtract r)) pitches
  pure chord

matchChordProfiles :: Profiles -> [Int] -> (Int, Double)
matchChordProfiles _        []      = (0, 0)
matchChordProfiles profiles pitches = (maxRoot, chordness)
 where
  roots = V.fromListN 12 [0 .. 11]
  n     = fromIntegral $ length pitches
  match chord prof = logDirichletMultinomial prof chord n
  bestMatch r = V.maximum $ match (mkChord r pitches) <$> profiles
  bestMatches = bestMatch <$> roots
  maxRoot     = V.maxIndex bestMatches
  chordness   = exp $ bestMatches V.! maxRoot
  check =
    if (chordness >= 1)
         || (chordness <= 0)
         || isNaN chordness
         || isInfinite chordness
      then "chordness = " <> show chordness <> " for pitches " <> show pitches
      else ""
