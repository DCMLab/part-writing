{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-|
Module      : VoiceLeading.Base
Description : Basic definitions for voice leading analysis
Copyright   : (c) Christoph Finkensiep, 2017
License     : MIT
Maintainer  : chfin@freenet.de
Stability   : experimental
Portability : POSIX

Voice leading rules describe restrictions and preferences regarding the simultaneous movement of two or more voices.
Most rules mention either configurations of voices at a single point in time (unary) or the movement of voices from one configuration to its successor (binary).

__Note: The following is not entirely up to date, refer to the individual documentation.__

This module provides basic types and functions for describing voice leading rules.
A piece of music is represented as a sequence of voice configurations called events.
An event is a mapping from voices to pitches and describes the pitch sounding in each voice at a given point in the piece.
As a special case, since a voice can have no pitch at all at some point, an event might also map a voice to a rest.
The 'Voice', 'Pitch', and, 'Event' types are used to represent voices, pitches, and events, respectively.

A piece is represented as a list of voice leading events, so the type synonyms 'Piece' and 'Pieces' for 'Lists's of 'Event's or 'Piece's, respectively, can be used for that.
Additionally, a type for extended events (which includes a "start" event and an "end" event) is provided by 'EEvent' and complemented by the corresponding 'EPiece' and 'EPieces' types.
These representations should be used in formal language or markov model contexts where the boundaries of an event sequence can be marked by "start" and "end" events.
-}
module VoiceLeading.Base (
    -- * Voices
    Voice(..)
  , ChoralVoice(..)
  , CounterpointVoice(..)
    -- * Pitches
  , Pitch(..)
  , pitchList
    -- ** Pitch Functions
  , isRest, isPitch, pitchHolds, pitchMidi
  , holdPitch, unholdPitch
    -- * Events
  , Event(..)
  , Beat
  , allEvents, eventList
    -- ** Event Functions
  , emptyEvent, isEmptyEvent, toEv
  , evGet, evGetMaybe, voices, pitches
  , removeRests, addRests, rests
    -- * Pieces
  , Piece(..), Pieces
  , normalizeTies, normalizeTiesScanner
    -- ** Piece Metadata
  , PieceMeta(..), nullPieceMeta
    -- *** Key signatures
  , KeySig(..), mkKeySig
    -- * Extended Events and Pieces
  -- , EEvent(..), EPiece, EPieces
  -- , toEPiece, toEPieces
  -- , eEventList
  ) where

import qualified Data.Map.Strict as M
import qualified Data.List as L
import qualified Debug.Trace as DT
import VoiceLeading.Helpers (showMap)

import Data.Function.Memoize (deriveMemoizable)

import GHC.Generics (Generic)
import Data.Hashable

-----------
-- Voice --
-----------

-- | The 'Voice' class represents voices.
class (Eq a, Ord a, Show a, Read a, Enum a, Bounded a) => Voice a where
  -- | A list of all 'Voice's.
  voiceList :: [a]
  defaultRange :: a -> (Int, Int)
  defaultRange v = (38,81) -- some kind of vocal range default values

-- | 'ChoralVoice' is an instance of 'Voice'.
data ChoralVoice = Bass | Tenor | Alto | Soprano
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic)

instance Voice ChoralVoice where
  voiceList = [Bass, Tenor, Alto, Soprano]
  defaultRange Bass    = (38,62)
  defaultRange Tenor   = (48,69)
  defaultRange Alto    = (55,74)
  defaultRange Soprano = (59,81)

instance Hashable ChoralVoice

data CounterpointVoice = LowCP | CF | HighCP
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic)

instance Voice CounterpointVoice where
  voiceList = [LowCP, CF, HighCP]

instance Hashable CounterpointVoice

-----------
-- Pitch --
-----------

-- | The 'Pitch' type represents a single pitch, which can be a rest or a midi pitch.
--   A pitch can be held from the previous note.
data Pitch = Rest | Pitch Int Bool
  deriving (Eq, Ord, Generic)

pitchNames = ["c", "c#", "d", "d#", "e", "f", "f#", "g", "g#", "a", "a#", "b"]
showPitch i = (pitchNames !! mod i 12) ++ show (div i 12 - 1)

instance Show Pitch where
  show Rest            = "R"
  show (Pitch i False) = showPitch i
  show (Pitch i True)  = '~' : showPitch i

instance Hashable Pitch

-- | A list of all 'Pitch'es.
pitchList :: [Pitch]
pitchList = Rest : map (\p -> Pitch p False) [41..79]

-- | Return 'True' if the given 'Pitch' is 'Rest'
isRest :: Pitch -> Bool
isRest Rest        = True
isRest (Pitch _ _) = False

-- | Return 'True' the given 'Pitch' is a midi pitch (i.e., not 'Rest')
isPitch :: Pitch -> Bool
isPitch Rest        = False
isPitch (Pitch _ _) = True

pitchHolds :: Pitch -> Bool
pitchHolds Rest        = False
pitchHolds (Pitch _ h) = h

pitchMidi :: Pitch -> Maybe Int
pitchMidi Rest        = Nothing
pitchMidi (Pitch i _) = Just i

holdPitch :: Pitch -> Pitch
holdPitch Rest = Rest
holdPitch (Pitch i _) = Pitch i True

unholdPitch :: Pitch -> Pitch
unholdPitch Rest = Rest
unholdPitch (Pitch i _) = Pitch i False

-----------
-- Event --
-----------

-- | The type 'Beat' is a bar-relative timestamp (alias for 'Int').
--   The beginning of the bar is encoded as 0 and higher values are proportional to time,
--   but otherwise the interpretation of a value is context-dependent.
type Beat = Rational

-- | The 'Event' type represents a single voice leading event.
--   An event is formalized as a mapping from voices to pitches,
--   so 'Event' has a strict 'Data.Map.Strict.Map' from 'Voice' to 'Pitch'
--   and a bar-relative timestamp.
data Event v = Event { evMap  :: (M.Map v Pitch)
                     , evBeat :: Beat }
  deriving (Eq, Generic)

instance Hashable v => Hashable (Event v) where
  hashWithSalt s (Event m b) =
    s `hashWithSalt` b `hashWithSalt` lst
    where lst = M.toAscList m

-- | The empty 'Event', containing no mapping from 'Voice's to 'Pitch'es.
emptyEvent :: Voice v => Event v
emptyEvent = addRests $ Event M.empty 0

isEmptyEvent :: Voice v => Event v -> Bool
isEmptyEvent (Event e _) = all isRest (M.elems e)

-- | Turns a 'Data.Map.Strict.Map' into an 'Event'.
toEv :: Voice v => M.Map v Pitch -> Beat -> Event v
toEv = Event

-- | Turns an 'Event' into a 'Data.Map.Strict.Map'.
-- fromEv :: Voice v => Event v -> M.Map v Pitch
-- fromEv (Event m) = m

-- | @evGetMaybe e v@ returns @'Just' p@ if the 'Event' @e@ maps 'Voice' @v@
--   to 'Pitch' @p@, 'Nothing' otherwise.
evGetMaybe :: Voice v => Event v -> v -> Maybe Pitch
evGetMaybe e v = M.lookup v (evMap e)
{-# INLINE evGetMaybe #-}

-- | Returns the 'Pitch' belonging to a 'Voice' in an 'Event'.
--   Returns 'Rest' if the voice is not found in the event.
evGet :: Voice v => Event v -> v -> Pitch
evGet e v = case evGetMaybe e v of
              (Just p) -> p
              Nothing  -> Rest
{-# INLINE evGet #-}

instance Voice v => Show (Event v) where
  show (Event m b) = "Event@" ++ show b  ++ showMap m

-- | Returns all 'Voice's in the 'Event'.
voices :: Voice v => Event v -> [v]
voices (Event m _) = M.keys m

-- | Returns all 'Pitch'es in the 'Event'.
pitches :: Voice v => Event v -> [Pitch]
pitches (Event m _) = M.elems m

-- | Returns a new 'Event' without mappings to 'Rest'.
removeRests :: Voice v => Event v -> Event v
removeRests (Event m b) = toEv (M.filter isPitch m) b

-- | Returns a new 'Event' containing a mapping from every 'Voice' in 'voiceList'.
--   Missing voices are mapped 'Rest'.
addRests :: Voice v => Event v -> Event v
addRests (Event m b) = toEv (foldl addRest m voiceList) b
  where addRest e v = if M.member v e
                      then e
                      else M.insert v Rest e

-- | Returns the number of 'Rest's in the 'Event'. 
rests :: Voice v => Event v -> Int
rests e = M.size (evMap e) - M.size (evMap (removeRests e))

-- | A list of all possible 'Event's
--   for given lists of 'Voice's and 'Pitch'es.
allEvents :: Voice v => [v] -> [Pitch] -> [Event v]
allEvents voices pitches = map makeEvent pitchProd
  where pitchProd = sequence $ replicate (length voices) pitches
        makeEvent = (flip toEv) 0 . M.fromList . zip voices

-- | A list of all possible 'Event's
--   derived from 'voiceList' and 'pitchList'.
eventList :: Voice v => [Event v]
eventList = allEvents voiceList pitchList

-----------
-- Piece --
-----------

-- | 'KeySig' represents a key signature given by a root (pitch class, c=0)
--   and a mode (mod 7): 0=ionian/major, 1=dorian, 2=phrygian, 3=lydian,
--   4=mixolydian, 5=aeolian/minor, 6=locrian 
data KeySig = KeySig
  { root :: Int
  , mode :: Int }
  deriving (Eq, Generic)

mkKeySig :: Int -> Int -> KeySig
mkKeySig r m = KeySig (mod r 12) (mod m 7)

instance Show KeySig where
  show (KeySig r m) = rn ++ '-' : mn
    where rn = ["c", "db", "d", "eb", "e", "f", "f#", "g", "ab", "a", "bb", "b"] !! mod r 12
          mn = ["major", "dorian", "phrygian", "lydian",
                "mixolydian", "minor", "locrian"] !! mod m 7

instance Hashable KeySig

$(deriveMemoizable ''KeySig)

-- | The type 'PieceMeta' holds metadata for a piece.
--   This metadata is used for improved export.
data PieceMeta = PieceMeta
  { title :: String
  , timeSignature :: (Integer, Integer)
  , keySignature :: KeySig }
  deriving (Show, Eq, Generic)

instance Hashable PieceMeta

nullPieceMeta = PieceMeta "untitled" (4,4) (KeySig 0 0)

-- | The type 'Piece' wraps '[Event]' and some metadata.
data Piece v = Piece
  { pieceMeta :: PieceMeta
  , pieceEvents :: [Event v]
  }
  deriving (Show, Generic)

instance Hashable v => Hashable (Piece v)

-- | The type 'Pieces' is a shortcut for '[Piece]'.
type Pieces v = [Piece v]

-- | Since each 'Pitch' carries both a pitch value and tie flag,
-- a pitch in one event might be held from the previous event
-- but have a different pitch value.
-- @normalizeTies piece keepTies@ fixes this.
-- If @keepTies@ is 'True', then all held pitches are adapted from left to right.
-- If @keepTies@ is 'False', then all ties between different pitch values are removed.
normalizeTies :: Voice v => Piece v -> Bool -> Piece v
normalizeTies (Piece meta evs) keep = Piece meta (scanl1 (normalizeTiesScanner keep) evs)

-- | Scanner ('scanl1') function for normalizing ties (cf. 'normalizeTies')
normalizeTiesScanner :: Voice v => Bool -> Event v -> Event v -> Event v
normalizeTiesScanner keep e1 (Event m b) = toEv (M.mapWithKey norm m) b
  where norm v p = norm1 (evGet e1 v) p
        norm1 (Pitch p1 _) (Pitch p2 True)
          | p1 /= p2 = if keep
                       then (Pitch p1 True)
                       else (Pitch p2 False)
        norm1 _ p = p

---------------------
-- Extended Events --
---------------------

-- -- | The type 'EEvent' extends 'Event' by a "start" (⋊) and an "end" (⋉) value.
-- --   This can be useful in situations where the beginning and the end of an
-- --   event sequence need to be taken into account.
-- data EEvent v = EStart | EEvent (Event v) | EEnd
--   deriving (Show)

-- deriving instance Eq v => Eq (EEvent v) 

-- -- | The type 'EPiece' is a list of 'EEvent's (like 'Piece' for 'Event')
-- type EPiece v = [EEvent v]

-- -- | Converts a sequence of 'Event's to a sequence of 'EEvent's,
-- --   enclosing it with 'EStart' and 'EEnd'.
-- toEPiece :: Voice v => Piece v -> EPiece v
-- toEPiece (Piece _ piece) = EStart : (map EEvent piece) ++ [EEnd]

-- -- | An 'EEvent' version of 'eventList' including 'EStart' and 'EEnd'.
-- eEventList :: Voice v => EPiece v
-- eEventList = toEPiece (Piece nullPieceMeta eventList)

-- -- | The type 'EPieces' is a sequence of 'EPiece's (like 'Pieces' for 'Piece')
-- type EPieces v = [EPiece v]

-- -- | Converts 'Pieces' to 'EPieces', adding 'EStarts' and 'EEnds'.
-- toEPieces :: Voice v => Pieces v -> EPieces v
-- toEPieces = map toEPiece
