{-# LANGUAGE ScopedTypeVariables #-}

module VoiceLeading.Automaton where

import VoiceLeading.Base
import VoiceLeading.Helpers
import VoiceLeading.IO.Midi

import qualified Data.Map as M
import Data.List (intercalate)

-----------------
-- State Model --
-----------------

data State v = State
  { aPrevPitch :: M.Map v [Pitch]
  , cpLastAcc :: Maybe (Event v)
  }

instance Show v => Show (State v) where
  show state = "S" ++ showMap (aPrevPitch state)

firstState :: forall v . Voice v => State v
firstState = let vs = voiceList :: [v] in
               State
               { aPrevPitch = M.fromList $ zip vs (repeat [])
               , cpLastAcc = Nothing }

nextState :: forall v . Voice v => State v -> Event v -> State v
nextState last ev = State
  { aPrevPitch = M.fromList $ map pushVoice vs
  , cpLastAcc = Nothing -- this is not clear so far
  }
  where vs = voiceList :: [v]
        preprev = aPrevPitch last
        pushVoice v = (v, push (M.findWithDefault [] v preprev) (evGet ev v))
        push lst (Pitch i True) = lst
        push lst p              = take 2 $ p : lst

pieceStates :: Voice v => Piece v -> [State v]
pieceStates (Piece _ evs) = scanl nextState firstState evs

---------------------
-- extended events --
---------------------

data EEvent v = EEvent
  { aPitch :: M.Map v Pitch
  , aBeat  :: Beat
  , aFirst :: Bool
  , aLast  :: Bool
  }

instance (Show v) => Show (EEvent v) where
  show (EEvent ps b f l) = "E" ++ fst ++ lst ++ "@" ++ show b ++ showMap ps
    where fst = if f then "⋊" else ""
          lst = if l then "⋉" else ""

extendEvent :: Event v -> EEvent v
extendEvent (Event m b) = EEvent m b False False

extend :: Piece v -> [EEvent v]
extend (Piece _ es) = markLast (markFirst (map extendEvent es))
  where markFirst []      = []
        markFirst (e:rst) = reverse (e { aFirst = True } : rst)
        markLast []       = []
        markLast (e:rst)  = reverse (e { aLast = True } : rst)

--------------------------
-- aspects / viewpoints --
--------------------------

-- TODO
