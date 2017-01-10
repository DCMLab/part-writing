{-|
Module      : VoiceLeading.Base
Description : Basic definitions for voice leading analysis
Copyright   : (c) Christoph Finkensiep, 2017
License     : MIT
Maintainer  : chfin@freenet.de
Stability   : experimental
Portability : POSIX

__TODO__ Here is a longer description of this module, containing some
commentary with @some markup@. __TODO__
-}
module VoiceLeading.Base
  ( Voice(..)
  , Pitch(..)
  , Event(..)
  , Piece, Pieces
  , voiceList
  , pitchList
  , isRest, isPitch
  , emptyEvent, toEv, fromEv
  , getEv, getEvMaybe, voices, pitches
  , removeRests, addRests, rests
  ) where

import qualified Data.Map.Strict as M
import qualified Data.List as L
import qualified Data.Vector as V

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
-- An event is formalized as a mapping from voices to pitches,
-- so 'Event' is a newtype for a strict 'Data.Map.Strict.Map' from 'Voice' to 'Pitch'.
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

-- | @getEvMaybe e v@ returns @'Just' p@ if the 'Event' @e@ maps 'Voice' @v@ to 'Pitch' @p@,
-- 'Nothing' otherwise.
getEvMaybe :: Event -> Voice -> Maybe Pitch
getEvMaybe e v = M.lookup v (fromEv e)

-- | Returns the 'Pitch' belonging to a 'Voice' in an 'Event'.
-- Returns 'Rest' if the voice is not found in the event.
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
-- Missing voices are mapped 'Rest'.
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
