{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module VoiceLeading.Automaton where

import           VoiceLeading.Base
import           VoiceLeading.Theory            ( findHarm )
import           VoiceLeading.Helpers           ( showMap )

import qualified Data.Map.Strict               as M
import           Data.List                      ( scanl' )
import           Data.Hashable
import           Data.Default

-----------------
-- State Model --
-----------------

newtype State v = State
  { sPrevPitch :: M.Map v [Pitch]
  }

instance Show v => Show (State v) where
  show state = "S" ++ showMap (sPrevPitch state)

memoryLength :: Int
memoryLength = 2

firstState :: forall  v . Voice v => State v
firstState =
  let vs = voiceList :: [v]
  in  State { sPrevPitch = M.fromList $ zip vs (repeat []) }

nextState :: forall  v . Voice v => State v -> Event v -> State v
nextState !prev !ev = State { sPrevPitch = M.fromList $ map pushVoice vs }
 where
  vs      = voiceList :: [v]
  preprev = sPrevPitch prev
  pushVoice v = (v, push (M.findWithDefault [] v preprev) (evGet ev v))
  push lst (Pitch _ True) = lst
  push lst p              = take memoryLength $ p : lst

pieceStates :: Voice v => Piece v -> [State v]
pieceStates (Piece _ evs) = scanl' nextState firstState evs

---------------------
-- extended events --
---------------------

data EEvent v = EEvent
  { ePitch :: !(M.Map v Pitch)
  , eBeat  :: !Beat
  , eFirst :: !Bool
  , eLast  :: !Bool
  } deriving (Eq)

instance (Show v) => Show (EEvent v) where
  show (EEvent ps b f l) = "E" ++ beg ++ end ++ "@" ++ show b ++ showMap ps
   where
    beg = if f then "⋊" else ""
    end = if l then "⋉" else ""

instance Hashable v => Hashable (EEvent v) where
  hashWithSalt s (EEvent m b f l) =
    s `hashWithSalt` b `hashWithSalt` f `hashWithSalt` l `hashWithSalt` lst
    where lst = M.toAscList m

extend :: Event v -> EEvent v
extend (Event m b) = EEvent m b False False

extendLike :: Event v -> EEvent v -> EEvent v
extendLike (Event m b) (EEvent _ _ f l) = EEvent m b f l

extendPiece :: Piece v -> [EEvent v]
extendPiece (Piece _ es) = markLast (markFirst (map extend es))
 where
  markFirst []        = []
  markFirst (e : rst) = reverse (e { eFirst = True } : rst)
  markLast []        = []
  markLast (e : rst) = reverse (e { eLast = True } : rst)

extract :: EEvent v -> Event v
extract ev = Event (ePitch ev) (eBeat ev)

extractPiece :: PieceMeta -> [EEvent v] -> Piece v
extractPiece meta evs = Piece meta (map extract evs)

-- | Returns the 'Pitch' belonging to a 'Voice' in an 'Event'.
--   Returns 'Rest' if the voice is not found in the event.
eevGet :: Voice v => EEvent v -> v -> Pitch
eevGet e v = M.findWithDefault Rest v $ ePitch e
{-# INLINE eevGet #-}

eevPitches :: EEvent v -> [Pitch]
eevPitches = M.elems . ePitch

-------------------------------
-- global / external context --
-------------------------------

data Context v = Context
  { cVoiceRange :: !(M.Map v (Int, Int))
  , cLeadingTone :: !Int
  , cKey :: !KeySig
  , cHarmEstimator :: !([Int] -> (Int, Double))
  }

mkDefaultCtx :: AutoOpts v -> KeySig -> Context v
mkDefaultCtx (AutoOpts range est) k@(KeySig r _) = Context
  { cVoiceRange    = range
  , cLeadingTone   = mod (r - 1) 12
  , cKey           = k
  , cHarmEstimator = est
  }

data AutoOpts v = AutoOpts
  { oVoiceRange :: !(M.Map v (Int, Int))
  , oHarmEstimator :: !([Int] -> (Int, Double))
  }

instance (Voice v) => Default (AutoOpts v) where
  def = AutoOpts
    { oVoiceRange    = M.fromList $ map (\v -> (v, defaultRange v)) voiceList
    , oHarmEstimator = findHarm
    }

---------------------------
-- running the automaton --
---------------------------

data AutoEnv v = AutoEnv
  { envEEvent :: !(EEvent v)
  , envState :: !(State v)
  , envContext :: !(Context v)
  }

runOnPiece :: (Voice v) => AutoOpts v -> Piece v -> (AutoEnv v -> a) -> [a]
runOnPiece opts piece@(Piece meta _) scanner = map
  scanner
  (zipWith3 AutoEnv eevs sts (repeat ctx))
 where
  eevs = extendPiece piece
  sts  = pieceStates piece
  ctx  = mkDefaultCtx opts (keySignature meta)
