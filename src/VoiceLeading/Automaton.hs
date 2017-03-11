{-# LANGUAGE ScopedTypeVariables #-}

module VoiceLeading.Automaton where

import VoiceLeading.Base
import VoiceLeading.Helpers
import VoiceLeading.IO.Midi

import qualified Data.Map.Strict as M
import Data.List (intercalate)
import Control.Monad.Reader

-----------------
-- State Model --
-----------------

data State v = State
  { sPrevPitch :: M.Map v [Pitch]
  , cpLastAcc :: Maybe (Event v)
  }

instance Show v => Show (State v) where
  show state = "S" ++ showMap (sPrevPitch state)

firstState :: forall v . Voice v => State v
firstState = let vs = voiceList :: [v] in
               State
               { sPrevPitch = M.fromList $ zip vs (repeat [])
               , cpLastAcc = Nothing }

nextState :: forall v . Voice v => State v -> Event v -> State v
nextState last ev = State
  { sPrevPitch = M.fromList $ map pushVoice vs
  , cpLastAcc = Nothing -- this is not clear so far
  }
  where vs = voiceList :: [v]
        preprev = sPrevPitch last
        pushVoice v = (v, push (M.findWithDefault [] v preprev) (evGet ev v))
        push lst (Pitch i True) = lst
        push lst p              = take 2 $ p : lst

pieceStates :: Voice v => Piece v -> [State v]
pieceStates (Piece _ evs) = scanl nextState firstState evs

---------------------
-- extended events --
---------------------

data EEvent v = EEvent
  { ePitch :: M.Map v Pitch
  , eBeat  :: Beat
  , eFirst :: Bool
  , eLast  :: Bool
  }

instance (Show v) => Show (EEvent v) where
  show (EEvent ps b f l) = "E" ++ fst ++ lst ++ "@" ++ show b ++ showMap ps
    where fst = if f then "⋊" else ""
          lst = if l then "⋉" else ""

extend :: Event v -> EEvent v
extend (Event m b) = EEvent m b False False

extendPiece :: Piece v -> [EEvent v]
extendPiece (Piece _ es) = markLast (markFirst (map extend es))
  where markFirst []      = []
        markFirst (e:rst) = reverse (e { eFirst = True } : rst)
        markLast []       = []
        markLast (e:rst)  = reverse (e { eLast = True } : rst)

extract :: EEvent v -> Event v
extract ev = Event (ePitch ev) (eBeat ev)

-------------------------------
-- global / external context --
-------------------------------

data Context v = Context
  { cVoiceRange :: M.Map v (Int, Int)
  , cLeadingTone :: Int
  , cKey :: KeySig
  }

mkDefaultCtx :: Voice v => KeySig -> Context v
mkDefaultCtx k@(KeySig r _) = Context
  { cVoiceRange  = M.fromList $ map (\v -> (v, defaultRange v)) voiceList
  , cLeadingTone = mod (r-1) 12
  , cKey         = k
  }

--------------------------
-- aspects / viewpoints --
--------------------------

-- pack state and event into a reader:

data AutoEnv v = AutoEnv
  { envEvent :: EEvent v
  , envState :: State v
  , envContext :: Context v
  }

type Aspect v a = Reader (AutoEnv v) a

-- | Evaluate a single aspect given an event and a state
evalAspect :: AutoEnv v -> Aspect v a -> a
evalAspect = flip runReader

-- | only for testing individual aspects
runAspect :: (Voice v) => Piece v -> Aspect v a -> [a]
runAspect piece@(Piece meta _) asp = map (runReader asp) (zipWith3 AutoEnv evs sts (repeat ctx))
  where evs = extendPiece piece
        sts = pieceStates piece
        ctx = mkDefaultCtx (keySignature meta)

-- aspects
----------

thisEvent :: Aspect v (EEvent v)
thisEvent = ask >>= return . envEvent

thisState :: Aspect v (State v)
thisState = ask >>= return . envState

thisContext :: Aspect v (Context v)
thisContext = ask >>= return . envContext

-- event

aFirst :: Aspect v Bool
aFirst = (liftM eFirst) thisEvent

aLast :: Aspect v Bool
aLast = (liftM eLast) thisEvent

aPitch :: Voice v => v -> Aspect v Pitch
aPitch v = thisEvent >>= (\ev -> return $ evGet (extract ev) v)

aBeat :: Aspect v Beat
aBeat = liftM eBeat thisEvent

-- state

aPrevPitch :: Ord v => v -> Int -> Aspect v Pitch
aPrevPitch v i = do
  state <- thisState
  let lst = (M.findWithDefault [] v (sPrevPitch state))
  return $  maybe Rest id (lGet lst i)

aLastAcc :: Voice v => v -> Aspect v Pitch
aLastAcc v = do
  state <- thisState
  return $ maybe Rest (flip evGet v) (cpLastAcc state)

-- context

aVoices :: Voice v => Aspect v [v]
aVoices = return voiceList

aVoiceRange :: Ord v => v -> Aspect v (Int, Int)
aVoiceRange v = do
  ctx <- thisContext
  return $ M.findWithDefault (minBound,maxBound) v (cVoiceRange ctx) 

aLeadingTone :: Aspect v Int
aLeadingTone = liftM cLeadingTone thisContext

aKey :: Aspect v KeySig
aKey = liftM cKey thisContext
