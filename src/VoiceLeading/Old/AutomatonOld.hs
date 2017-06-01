module VoiceLeading.AutomatonOld where

import VoiceLeading.Base
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

import Debug.Trace as DT

-- VL automaton interface
-------------------------

data VLAutomaton a v = VLAutomaton
  { start   :: a
  , accepts :: a -> Bool
  , fails   :: a -> Bool
  , trans   :: a -> EEvent v -> a
  }

-- VL automaton state datastructure
-----------------------------------

type Memory v = M.Map v [Pitch]

data VLState v = Start | Accept | Fail
               | VLState Bool (Memory v) -- VLState first mem
  deriving (Eq, Show)

mkVLStateAuto :: Voice v => (VLState v -> EEvent v -> VLState v) -> VLAutomaton (VLState v) v
mkVLStateAuto trans = VLAutomaton
                      { start   = Start
                      , accepts = (==Accept)
                      , fails   = (==Fail)
                      , trans   = trans
                      }

stateAuto :: Voice v => VLAutomaton (VLState v) v
stateAuto = mkVLStateAuto $ stateTrans strictRules strictEndRules

-- instance Voice v => VLAutomaton (VLState v) v where
--   start   = Start
--   accepts = (==Accept)
--   fails   = (==Fail)
--   trans :: Voice v => VLState v -> EEvent v -> VLState v
--   trans   = stateTrans

stateHolds :: Voice v => VLState v -> Event v
stateHolds (VLState _ m) = toEv $ M.filter pitchHolds (M.map headPitch m)
  where headPitch (p:ps) = p
        headPitch []     = Rest
stateHolds _             = emptyEvent

-- strict VL rules
------------------

type StrictRule v = VLState v -> Event v -> Bool

strictRules :: Voice v => [StrictRule v]
strictRules = [consistency]

consistency :: Voice v => StrictRule v
consistency s e = all comp (voices holds)
  where holds  = stateHolds s
        comp v = (getEvMaybe e v >>= pitchMidi) == (getEvMaybe holds v >>= pitchMidi)

type StrictEndRule v = VLState v -> Bool

strictEndRules :: Voice v => [StrictEndRule v]
strictEndRules = [consistentEnd]

consistentEnd :: Voice v => StrictEndRule v
consistentEnd s = isEmptyEvent (stateHolds s)

-- state transition
-------------------

firstState :: Voice v => VLState v
firstState = VLState True (M.fromList $ map mkMem voiceList)
  where mkMem v = (v, [Rest, Rest, Rest])

stateTrans :: Voice v => [StrictRule v] -> [StrictEndRule v] ->
              VLState v -> EEvent v -> VLState v
stateTrans rs ers = t
  where t Fail _       = Fail
        t Start EStart = firstState
        t Start _      = Fail
        t Accept _     = Fail
        t _ EStart     = Fail
        t s EEnd       = if mayEnd ers s then Accept else Fail
        t s (EEvent e) | checkTrans rs s e = nextState s e
        t  _ _          = Fail

mayEnd :: Voice v => [StrictEndRule v] -> VLState v -> Bool
mayEnd ers s@(VLState _ _) = all ($s) ers
mayEnd _ _ = True

checkTrans :: Voice v => [StrictRule v] -> VLState v -> Event v -> Bool
checkTrans rs s@(VLState _ _) e = all (\r -> r s e) rs
checkTrans _ _ _ = True

nextState :: Voice v => VLState v -> Event v -> VLState v
nextState (VLState _ m) e = VLState False (M.mapWithKey updt m)
  where updt v ps@(p:_) | pitchHolds p = (getEv e v) : (take 2 (tail ps))
        updt v ps                      = (getEv e v) : (take 2 ps)

-- run automaton
----------------

checkPiece :: (Voice v) => VLAutomaton a v -> EPiece v -> a
checkPiece a p = foldl (trans a) (start a) p

analysePiece :: (Voice v) => VLAutomaton a v -> EPiece v -> [a]
analysePiece a p = scanl (trans a) (start a) p
