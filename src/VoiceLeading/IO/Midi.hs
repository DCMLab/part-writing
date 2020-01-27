{-# LANGUAGE ScopedTypeVariables #-}

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
  , corpusPaths
  , corpusPieces
  , corpusDir
  )
where

import           VoiceLeading.Base
import           Codec.Midi
import qualified Data.Map.Strict               as M
import           Control.Applicative            ( liftA2 )
import           Control.Monad                  ( filterM )
import           Data.List                      ( groupBy
                                                , dropWhileEnd
                                                , find
                                                )
import           Data.Function                  ( on )
import           Data.Ratio
import           System.FilePath                ( takeBaseName )
import           System.Directory               ( listDirectory )

isTimeSig :: Message -> Bool
isTimeSig TimeSignature{} = True
isTimeSig _               = False

isKeySig :: Message -> Bool
isKeySig (KeySignature _ _) = True
isKeySig _                  = False

ppq :: TimeDiv -> Int
ppq (TicksPerBeat t    ) = t
ppq (TicksPerSecond _ _) = 480 -- no luck, just guess.
                               -- convert tracks in this case?

tickToBeat :: Int -> Int -> Ticks -> Beat
tickToBeat bardur ppb t =
  let rel = mod t bardur in (fromIntegral rel % fromIntegral ppb)

-- | Load a MIDI file and return a 'Piece'.
-- Use a type assertion in cases, where the voice type @v@ is not clear, e.g.:
-- > piece <- loadMidi "..." :: IO (Piece ChoralVoice)
loadMidi :: Voice v => FilePath -> IO (Piece v)
loadMidi fp = do
  midi <- importFile fp
  case midi of
    Left  _ -> return $ Piece nullPieceMeta []
    Right m -> return $ convertMidi fp m

findTS :: Track Ticks -> (Int, Int)
findTS track = getTS $ find (isTimeSig . snd) track
 where
  getTS (Just (_, (TimeSignature bpbar denom _ _))) = (bpbar, denom)
  getTS _ = (4, 2)

findKS :: Track Ticks -> (Int, Int)
findKS track = getKS $ find (isKeySig . snd) track
 where
  getKS (Just (_, (KeySignature accs mode))) = (accs, mode)
  getKS _ = (0, 0)

convertMidi :: forall  v . Voice v => FilePath -> Midi -> Piece v
convertMidi fp midi =
  let s              = midiToStream midi (length (voiceList :: [v]))
      notes          = filter (liftA2 (||) isNoteOn isNoteOff . snd) s
      groups         = groupBy ((==) `on` fst) notes
      (bpbar, denom) = findTS s
      qpbar          = 4 * (bpbar % 2 ^ denom)
      bardur         = round $ fromIntegral (ppq (timeDiv midi)) * qpbar -- bar duration in ticks
      bdur           = round $ bardur % bpbar -- beat duration in ticks
      t2b            = tickToBeat bardur bdur -- converts abs ticks to rel beats
      (accs, mode)   = findKS s
      events         = scanl (msgToEvent t2b) emptyEvent groups
      evdrop         = dropWhileEnd isEmptyEvent (dropWhile isEmptyEvent events)
      meta = PieceMeta { title         = takeBaseName fp
        -- clever key signature conversion calculations:
        -- root = 7*accidentals (mod 12)
        -- modus = 0*5 = 0 (major) or 1*5 = 5 (minor)
                       , keySignature  = mkKeySig (mod (7 * accs) 12) (mode * 5)
                       , timeSignature = (toInteger bpbar, 2 ^ denom)
                       }
  in  Piece meta evdrop

midiToStream :: Midi -> Int -> Track Ticks
midiToStream m n = toAbsTime $ foldl1 merge (take (n + 1) (tracks m))

channelToVoice :: Voice v => Int -> v
channelToVoice c =
  let vl = voiceList
      i  = length vl - c - 1
  in  if i < 0 then error "Not enough Voices for channels." else vl !! i

msgToEvent
  :: forall v
   . Voice v
  => (Ticks -> Beat)
  -> Event v
  -> [(Ticks, Message)]
  -> Event v
msgToEvent ttb (Event pm _) msgs = toEv (foldl applyMsg holdAll msgs) beat
 where
  holdAll = M.map holdPitch pm
  beat    = ttb (fst (head msgs))
  nv      = length (voiceList :: [v])
  applyMsg m (_, NoteOff c _ _) =
    if c >= nv then m else M.insert (channelToVoice c) Rest m
  applyMsg m (_, NoteOn c _ 0) =
    if c >= nv then m else M.insert (channelToVoice c) Rest m
  applyMsg m (_, NoteOn c p _) =
    if c >= nv then m else M.insert (channelToVoice c) (Pitch p False) m
  applyMsg m _ = m

------------------------
-- pieces and corpora --
------------------------

corpusDir :: FilePath
corpusDir = "data/corpus/"

is4Choral :: FilePath -> IO Bool
is4Choral fp = do
  midi <- importFile fp
  case midi of
    Left  _ -> pure False
    Right m -> pure $ length (tracks m) == 5

corpusPaths :: IO [FilePath]
corpusPaths = do
  files <- listDirectory corpusDir
  let paths = map (corpusDir ++) files
  filterM is4Choral paths

corpusPieces :: IO [Piece ChoralVoice]
corpusPieces = do
  paths <- corpusPaths
  mapM loadMidi paths

testPiece :: IO (Piece ChoralVoice)
testPiece = do
  (Piece meta evs) <- loadMidi "data/testpiece_bwv269.mid"
  return $ Piece (meta { title = "Aus meines Herzens Grunde" }) evs
