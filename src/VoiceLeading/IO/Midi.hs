{-|
Module      : VoiceLeading.IO.Midi
Description : Basic definitions for voice leading analysis
Copyright   : (c) Christoph Finkensiep, 2017
License     : MIT
Maintainer  : chfin@freenet.de
Stability   : experimental
Portability : POSIX

Load and (in the future) save Midi files to / from the VL representation.
-}
module VoiceLeading.IO.Midi
  ( loadMidi
  , testPiece
  ) where

import VoiceLeading.Base
import Codec.Midi
import qualified Data.Map.Strict as M
import Control.Applicative (liftA2)
import Data.List (groupBy, dropWhileEnd, find)
import Data.Function (on)
import Data.Ratio
import System.FilePath (takeBaseName)

choraleFP :: String -> FilePath
choraleFP chorale = "/home/chfin/Uni/master/data/JSB Chorales/" ++ chorale ++ ".mid"

chorale = "/home/chfin/Uni/master/data/bach_chorales_bachcentral/01AusmeinesHerz.mid"

isTimeSig :: Message -> Bool
isTimeSig (TimeSignature _ _ _ _) = True
isTimeSig _ = False

isKeySig :: Message -> Bool
isKeySig (KeySignature _ _) = True
isKeySig _ = False

ppq :: TimeDiv -> Int
ppq (TicksPerBeat t) = t
ppq (TicksPerSecond _ _) = 480 -- no luck, just guess.
                               -- convert tracks in this case?

tickToBeat :: Int -> Int -> Ticks -> Beat
tickToBeat bardur ppb t = let rel = mod t bardur in
                            (fromIntegral rel % fromIntegral ppb)

-- | Load a MIDI file and return a 'Piece'.
-- Use a type assertion in cases, where the voice type @v@ is not clear, e.g.:
-- > piece <- loadMidi "..." :: IO (Piece ChoralVoice)
loadMidi :: Voice v => FilePath -> IO (Piece v)
loadMidi fp = do
  midi <- importFile fp
  case midi of
    Left _  -> return $ Piece nullPieceMeta []
    Right m -> return $ convertMidi fp m
      
convertMidi :: Voice v => FilePath -> Midi -> (Piece v)
convertMidi fp midi =
  let s      = (midiToStream midi)
      notes  = filter (liftA2 (||) isNoteOn isNoteOff . snd) s
      groups = groupBy ((==) `on` fst) notes
      TimeSignature bpbar denom _ _ =
        maybe (TimeSignature 4 2 24 8) snd (find (isTimeSig . snd) s)
      qpbar  = 4 * (bpbar % 2^denom)
      bardur = round $ fromIntegral (ppq (timeDiv midi)) * qpbar
      ttb    = tickToBeat bardur (ppq (timeDiv midi))
      KeySignature accs key =
        maybe (KeySignature 0 0) snd (find (isKeySig . snd) s)
      events = scanl (msgToEvent ttb) emptyEvent groups
      evdrop = dropWhileEnd isEmptyEvent (dropWhile isEmptyEvent events)
      meta   = PieceMeta
        { title = takeBaseName fp
        -- clever key signature conversion calculations:
        -- root = 7*accidentals (mod 12)
        -- modus = 0*5 = 0 (major) or 1*5 = 5 (minor)
        , keySignature = mkKeySig (mod (7*accs) 12) (key * 5)
        , timeSignature = (toInteger bpbar, 2^denom) }
  in
    Piece meta evdrop

midiToStream :: Midi -> Track Ticks
midiToStream m = toAbsTime $ foldl1 merge (tracks m)

channelToVoice :: Voice v => Int -> v
channelToVoice c = let vl = voiceList
                       l = length vl in
                     vl !! (l - c - 1)

msgToEvent :: Voice v => (Ticks -> Beat) -> Event v -> [(Ticks,Message)] -> Event v
msgToEvent ttb (Event pm pb) msgs = toEv (foldl applyMsg holdAll msgs) beat
  where holdAll = M.map holdPitch pm
        beat = ttb (fst (head msgs))
        applyMsg m (_,NoteOff c _ _) = M.insert (channelToVoice c) Rest m
        applyMsg m (_,NoteOn c p _)  = M.insert (channelToVoice c) (Pitch p False) m

testPiece :: IO (Piece ChoralVoice)
testPiece = loadMidi "01AusmeinesHerz.mid"
