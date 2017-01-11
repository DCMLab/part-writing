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

This module provides basic types and functions for describing voice leading rules.
A piece of music is represented as a sequence of voice configurations called events.
An event is a mapping from voices to pitches and describes the pitch sounding in each voice at a given point in the piece.
As a special case, since a voice can have no pitch at all at some point, an event might also map a voice to a rest.
The 'Voice', 'Pitch', and, 'Event' types are used to represent voices, pitches, and events, respectively.

A piece is represented as a list of voice leading events, so the type synonyms 'Piece' and 'Pieces' for 'Data.Vector.Vector's of 'Event's or 'Piece's, respectively, can be used for that.
Additionally, a type for extended events (which includes a "start" event and an "end" event) is provided by 'EEvent' and complemented by the corresponding 'EPiece' and 'EPieces' types.
These representations should be used in formal language or markov model contexts where the boundaries of an event sequence can be marked by "start" and "end" events.
-}
module VoiceLeading.Base
  ( Voice(..)
  , Pitch(..)
  , Event(..)
  , Piece, Pieces
  , EEvent(..), EPiece, EPieces
  , voiceList
  , pitchList
  , isRest, isPitch
  , emptyEvent, toEv, fromEv
  , getEv, getEvMaybe, voices, pitches
  , removeRests, addRests, rests
  , allEvents, eventList
  , toEPiece, toEPieces
  , eEventList
  ) where

import qualified Data.Map.Strict as M
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Debug.Trace as DT

-----------
-- Voice --
-----------

-- | The 'Voice' type represents voices.
data Voice = Bass | Tenor | Alto | Soprano
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

-- | A list of all 'Voice's.
voiceList :: [Voice]
voiceList = [Bass, Tenor, Alto, Soprano]

-----------
-- Pitch --
-----------

-- | The 'Pitch' type represents a single pitch, which can be a rest or a midi pitch.
data Pitch = Rest | Pitch Int
  deriving (Show, Eq, Ord)

-- | A list of all 'Pitch'es.
pitchList :: [Pitch]
pitchList = Rest : map Pitch [29..67]

-- | Return 'True' if the given 'Pitch' is 'Rest'
isRest :: Pitch -> Bool
isRest Rest = True
isRest (Pitch _) = False

-- | Return 'True' the given 'Pitch' is a midi pitch (i.e., not 'Rest')
isPitch :: Pitch -> Bool
isPitch Rest = True
isPitch (Pitch _) = False

-----------
-- Event --
-----------

-- | The 'Event' type represents a single voice leading event.
--   An event is formalized as a mapping from voices to pitches,
--   so 'Event' is a newtype for a strict 'Data.Map.Strict.Map'
--   from 'Voice' to 'Pitch'.
newtype Event = Event (M.Map Voice Pitch)

-- | The empty 'Event', containing no mapping from 'Voice's to 'Pitch'es.
emptyEvent :: Event
emptyEvent = addRests $ Event M.empty

-- | Turns a 'Data.Map.Strict.Map' into an 'Event'.
toEv :: M.Map Voice Pitch -> Event
toEv = Event

-- | Turns an 'Event' into a 'Data.Map.Strict.Map'.
fromEv :: Event -> M.Map Voice Pitch
fromEv (Event m) = m

-- | @getEvMaybe e v@ returns @'Just' p@ if the 'Event' @e@ maps 'Voice' @v@
--   to 'Pitch' @p@, 'Nothing' otherwise.
getEvMaybe :: Event -> Voice -> Maybe Pitch
getEvMaybe e v = M.lookup v (fromEv e)

-- | Returns the 'Pitch' belonging to a 'Voice' in an 'Event'.
--   Returns 'Rest' if the voice is not found in the event.
getEv :: Event -> Voice -> Pitch
getEv e v = case getEvMaybe e v of
              (Just p) -> p
              Nothing  -> Rest

instance Show Event where
  show e = "Event{" ++ (L.intercalate ", " pairs) ++ "}"
    where pairs = map (\ (v,p) -> show v ++ ": " ++ show p) (M.toList $ fromEv e)

-- | Returns all 'Voice's in the 'Event'.
voices :: Event -> [Voice]
voices e = M.keys $ fromEv e

-- | Returns all 'Pitch'es in the 'Event'.
pitches :: Event -> [Pitch]
pitches e = M.elems $ fromEv e

-- | Returns a new 'Event' without mappings to 'Rest'.
removeRests :: Event -> Event
removeRests e = toEv $ M.filter isPitch (fromEv e)

-- | Returns a new 'Event' containing a mapping from every 'Voice' in 'voiceList'.
--   Missing voices are mapped 'Rest'.
addRests :: Event -> Event
addRests e = toEv $ foldl addRest (fromEv e) voiceList
  where addRest e v = if M.member v e
                      then e
                      else M.insert v Rest e

-- | Returns the number of 'Rest's in the 'Event'. 
rests :: Event -> Int
rests e = M.size (fromEv e) - M.size (fromEv (removeRests e))

-- | The type 'Piece' is a shortcut for a 'Data.Vector.Vector' of 'Event's.
type Piece = V.Vector Event

-- | The type 'Pieces' is a shortcut for a 'Data.Vector.Vector' of 'Piece's.
type Pieces = V.Vector Piece

-- | A 'Data.Vector.Vector' of all possible 'Event's
--   for given lists of 'Voice's and 'Pitch'es.
allEvents :: [Voice] -> [Pitch] -> Piece
allEvents voices pitches = V.map makeEvent pitchProd
  where pitchProd = V.fromList $ sequence $ replicate (length voices) pitches
        makeEvent = toEv. M.fromList . zip voices

-- | A 'Data.Vector.Vector' of all possible 'Event's
--   derived from 'voiceList' and 'pitchList'.
eventList :: Piece
eventList = allEvents voiceList pitchList

---------------------
-- Extended Events --
---------------------

-- | The type 'EEvent' extends 'Event' by a "start" and an "end" value.
--   This can be useful in situations where the beginning and the end of an
--   event sequence need to be taken into account.
data EEvent = EStart | EEvent Event | EEnd
  deriving (Show)

-- | The type 'EPiece' is a sequence of 'EEvent's (like 'Piece' for 'Event')
type EPiece = V.Vector EEvent

-- | Converts a sequence of 'Event's to a sequence of 'EEvent's,
--   enclosing it with 'EStart' and 'EEnd'.
toEPiece :: Piece -> EPiece
toEPiece piece = (V.singleton EStart) V.++
                 (V.map EEvent piece) V.++
                 (V.singleton EEnd)

-- | An 'EEvent' version of 'eventList' including 'EStart' and 'EEnd'.
eEventList :: EPiece
eEventList = toEPiece eventList

-- | The type 'EPieces' is a sequence of 'EPiece's (like 'Pieces' for 'Piece')
type EPieces = V.Vector EPiece

-- | Converts 'Pieces' to 'EPieces', adding 'EStarts' and 'EEnds'.
toEPieces :: Pieces -> EPieces
toEPieces = V.map toEPiece
