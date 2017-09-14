{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module VoiceLeading.Automaton where

import VoiceLeading.Base
import VoiceLeading.Theory
import VoiceLeading.Helpers
import VoiceLeading.IO.Midi

import qualified Data.Map.Strict as M
import Data.List (intercalate, nub)
import qualified Data.Text as T
import Data.Maybe (mapMaybe)
import Data.Semigroup ((<>))
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Trans.State as ST
import qualified Control.Lens as L
import Data.Hashable

-----------------
-- State Model --
-----------------

data State v = State
  { sPrevPitch :: M.Map v [Pitch]
  , cpLastAcc :: Maybe (Event v)
  }

instance Show v => Show (State v) where
  show state = "S" ++ showMap (sPrevPitch state)

memoryLength :: Int
memoryLength = 2

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
        push lst p              = take memoryLength $ p : lst

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
  where markFirst []      = []
        markFirst (e:rst) = reverse (e { eFirst = True } : rst)
        markLast []       = []
        markLast (e:rst)  = reverse (e { eLast = True } : rst)

extract :: EEvent v -> Event v
extract ev = Event (ePitch ev) (eBeat ev)

extractPiece :: PieceMeta -> [EEvent v] -> Piece v
extractPiece meta evs = Piece meta (map extract evs)

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

---------------------------
-- running the automaton --
---------------------------

data AutoEnv v = AutoEnv
  { envEvent :: EEvent v
  , envState :: State v
  , envContext :: Context v
  }

runOnPiece :: (Voice v) => Piece v -> (AutoEnv v -> a) -> [a]
runOnPiece piece@(Piece meta _) scanner =
  map scanner (zipWith3 AutoEnv evs sts (repeat ctx))
  where evs = extendPiece piece
        sts = pieceStates piece
        ctx = mkDefaultCtx (keySignature meta)

--------------------------
-- aspects / viewpoints --
--------------------------

-- types
--------

type Interval = Int

interval :: Int -> Int -> Interval
interval = flip (-)

data Motion = Stationary | Oblique | Parallel | Similar | Contrary
  deriving (Show, Eq)

motion :: Int -> Int -> Int -> Int -> Motion
motion v1p1 v1p2 v2p1 v2p2
  | v1p1 == v1p2 && v2p1 == v2p2      = Stationary
  | v1p1 == v1p2 || v2p1 == v2p2      = Oblique
  | v2p1 - v1p1 == v2p2 - v1p2        = Parallel
  | (v1p2 - v1p1) * (v2p2 - v2p1) > 0 = Similar
  | otherwise                         = Contrary

-- memoization
--------------

data AspectMem v = AspectMem
                   { _memPitches :: Maybe [Int]
                   , _memPrevPitchI :: M.Map (v,Int) Int
                   , _memMotion :: M.Map (v,v) Motion
                   , _memRoot :: Maybe (Int, Double) }
  deriving Show

L.makeLenses ''AspectMem

emptyMem = AspectMem Nothing M.empty M.empty Nothing

remember :: L.Lens' (AspectMem v) (Maybe a) -> Aspect v a -> Aspect v a
remember lens val = do
  mem <- lift ST.get
  case L.view lens mem of
    Just x -> pure x
    Nothing -> do
      res <- val
      lift $ ST.put (L.set lens (Just res) mem)
      pure res

-- aspect type
--------------

-- pack state and event into a reader, wrapped by a memoization state and maybe for failure:

type Aspect v = MaybeT (ST.StateT (AspectMem v) (Reader (AutoEnv v)))

-- | Evaluate a single aspect given an event and a state
runAspect :: AspectMem v -> Aspect v a -> AutoEnv v -> (Maybe a, AspectMem v)
runAspect mem = runReader . (flip ST.runStateT mem) . runMaybeT

-- | only for testing individual aspects
runAspectOn :: (Voice v) => Piece v -> Aspect v a -> [Maybe a]
runAspectOn piece asp = runOnPiece piece (fst . runAspect emptyMem asp)

-- aspects
----------

thisEEvent :: Aspect v (EEvent v)
thisEEvent = envEvent <$> ask

thisEvent :: Aspect v (Event v)
thisEvent = extract <$> thisEEvent

thisState :: Aspect v (State v)
thisState = envState <$> ask

thisContext :: Aspect v (Context v)
thisContext = envContext <$> ask

-- event

pitchMidiA :: Aspect v Pitch -> Aspect v Int
pitchMidiA p = p >>= (liftMaybe . pitchMidi)

aFirst :: Aspect v Bool
aFirst = eFirst <$> thisEEvent

aLast :: Aspect v Bool
aLast = eLast <$> thisEEvent

aPitch :: Voice v => v -> Aspect v Pitch
aPitch v = evGet <$> thisEvent <*> pure v

aPitchI :: Voice v => v -> Aspect v Int
aPitchI = pitchMidiA . aPitch

aBeat :: Aspect v Beat
aBeat = eBeat <$> thisEEvent

aPitches :: Aspect v [Pitch]
aPitches = nub <$> filter isPitch <$> pitches <$> thisEvent

aPitchesI :: Aspect v [Int]
aPitchesI = do
  ps <- aPitches
  remember memPitches $ pure (mapMaybe pitchMidi ps)
  

-- state

aPrevPitch :: Ord v => v -> Int -> Aspect v Pitch
aPrevPitch v i = do
  state <- thisState
  let lst = (M.findWithDefault [] v (sPrevPitch state))
  pure $ maybe Rest id (lGet lst i)

aPrevPitchI :: Ord v => v -> Int -> Aspect v Int
aPrevPitchI v i = remember (memPrevPitchI . L.at (v,i)) $
                  pitchMidiA (aPrevPitch v i)

aLastAcc :: Voice v => v -> Aspect v Pitch
aLastAcc v = do
  state <- thisState
  pure $ maybe Rest (flip evGet v) (cpLastAcc state)

aLastAccI :: Voice v => v -> Aspect v Int
aLastAccI = pitchMidiA . aLastAcc

-- context

aVoices :: Voice v => Aspect v [v]
aVoices = pure voiceList

aVoiceRange :: Ord v => v -> Aspect v (Int, Int)
aVoiceRange v = do
  ctx <- thisContext
  pure $ M.findWithDefault (minBound,maxBound) v (cVoiceRange ctx) 

aLeadingTone :: Aspect v Int
aLeadingTone = cLeadingTone <$> thisContext

aKey :: Aspect v KeySig
aKey = cKey <$> thisContext

-- derived aspects

aBVI :: Voice v => v -> v -> Aspect v Interval
aBVI v1 v2 = interval <$> aPitchI v1 <*> aPitchI v2

aWVI :: Voice v => v -> Aspect v Interval
aWVI v = interval <$> aPrevPitchI v 0 <*> aPitchI v

aPrevBVI :: Voice v => v -> v -> Aspect v Interval
aPrevBVI v1 v2 = interval <$> aPrevPitchI v1 0 <*> aPrevPitchI v2 0

aPrevWVI :: Voice v => v -> Aspect v Interval
aPrevWVI v = interval  <$> aPrevPitchI v 1 <*> aPrevPitchI v 0

aCrossInt :: Voice v => v -> v -> Aspect v Interval
aCrossInt v1 v2 = interval <$> aPrevPitchI v1 0 <*> aPitchI v2

aRest :: Voice v => v -> Aspect v Bool
aRest v = isRest <$> aPitch v

aHolds :: Voice v => v -> Aspect v Bool
aHolds v = pitchHolds <$> aPitch v

aMotion :: Voice v => v -> v -> Aspect v Motion
aMotion v1 v2 = remember (memMotion . L.at (v1,v2)) $
                motion
                <$> aPrevPitchI v1 0 <*> aPitchI v1
                <*> aPrevPitchI v2 0 <*> aPitchI v2

aRootI :: Aspect v (Int, Double)
aRootI = remember memRoot $ findRoot <$> aPitchesI

--------------
-- features --
--------------

type Feature v = (Aspect v Double)

-- helpers

fOver :: (v -> Feature v) -> [v] -> [Feature v]
fOver = map

fOver2 :: (v -> v -> Feature v) -> [(v,v)] -> [Feature v]
fOver2 = map . uncurry

bin :: Bool -> Double
bin True  = 1
bin False = 0

pureBin :: Applicative f => Bool -> f Double
pureBin = pure . bin

dist12 :: Int -> Double
dist12 dist
  | dist > 12 = fromIntegral (dist - 12)
  | otherwise = 0

-- actual features

fOutOfRange :: Voice v => v -> Feature v
fOutOfRange v = do
  (l,u) <- aVoiceRange v
  p     <- aPitchI v
  let distance
        | p < l = l - p
        | p > u = p - u
        | otherwise = 0
  pure $ fromIntegral distance -- alternative: function over distance

fTooFarSA :: Feature ChoralVoice
fTooFarSA = dist12 <$> aBVI Alto Soprano
-- alternative: function over distance

fTooFarAT :: Feature ChoralVoice
fTooFarAT = dist12 <$> aBVI Tenor Alto
-- alternative: function over distance

fUnison :: Voice v => v -> v -> Feature v
fUnison v1 v2 = bin <$> (==0) <$> aBVI v1 v2

fCommonTone :: Voice v => v -> Feature v
fCommonTone v = do
  pp  <- aPrevPitchI v 0
  ps  <- aPitchesI
  wvi <- aWVI v
  pureBin $ pp `elem` ps && wvi /= 0

fNearestTone :: Voice v => v -> Feature v
fNearestTone v = bin <$> or <$> (aVoices >>= mapM violated)
  where violated v2 = (<) <$> (abs <$> aCrossInt v v2) <*> (abs <$> aWVI v)
  -- true if rule is violated

fMelodyStays :: Voice v => v -> Feature v
fMelodyStays v = bin <$> ((&&) <$> ((==0) <$> aWVI v) <*> (not <$> aHolds v))

fMelodyStep :: Voice v => v -> Feature v
fMelodyStep v = do
  int <- abs <$> aWVI v
  pureBin $ int > 0 && int <= 2

fMelodySkip :: Voice v => v -> Feature v
fMelodySkip v = do
  int <- abs <$> aWVI v
  pureBin $ int > 2 && int <= 4

fMelodyLeap :: Voice v => v -> Feature v
fMelodyLeap v = do
  int <- abs <$> aWVI v
  pureBin $ int > 4 && int <= 12

fMelodyOverleap :: Voice v => v -> Feature v
fMelodyOverleap v = dist12 <$> (abs <$> aWVI v)
-- alternative: function over distance

fCrossing :: Voice v => v -> v -> Feature v
fCrossing v1 v2 = do
  bvi <- aBVI v1 v2
  pureBin $ v1 < v2 && bvi < 0 || v1 > v2 && bvi > 0

fOverlap :: Voice v => v -> v -> Feature v
fOverlap v1 v2 = do
  c12 <- aCrossInt v1 v2
  c21 <- aCrossInt v2 v1
  pureBin $ v1 < v2 && not (c12 >= 0 && c21 <= 0)
    || v1 > v2 && not (c12 <= 0 && c21 >= 0)

fParUnison :: Voice v => v -> v -> Feature v
fParUnison v1 v2 = do
  mot <- aMotion v1 v2
  bvi <- aBVI v1 v2
  pureBin $ mot == Parallel && bvi == 0

fParFifth :: Voice v => v -> v -> Feature v
fParFifth v1 v2 = do
  mot <- aMotion v1 v2
  bvi <- aBVI v1 v2
  pureBin $ mot == Parallel && mod (abs bvi) 12 == 7

fParOctave :: Voice v => v -> v -> Feature v
fParOctave v1 v2 = do
  mot <- aMotion v1 v2
  bvi <- aBVI v1 v2
  pureBin $ mot == Parallel && bvi /= 0 && abs bvi `mod` 12 == 0

fConsFifth :: Voice v => v -> v -> Feature v
fConsFifth v1 v2 = do
  mot  <- aMotion v1 v2
  bvi  <- aBVI v1 v2
  pbvi <- aPrevBVI v1 v2
  pureBin $ mot == Contrary && abs bvi `mod` 12 == 7 && abs pbvi `mod` 12 == 7

fConsOctave :: Voice v => v -> v -> Feature v
fConsOctave v1 v2 = do
  mot  <- aMotion v1 v2
  bvi  <- aBVI v1 v2
  pbvi <- aPrevBVI v1 v2
  pureBin $ mot == Contrary && bvi /= 0 && abs bvi `mod` 12 == 0 && abs pbvi `mod` 12 == 0

fHiddenFifth :: Voice v => v -> v -> Feature v
fHiddenFifth vlower vupper = do
  mot  <- aMotion vlower vupper
  bvi  <- aBVI vlower vupper
  uwvi <- aWVI vupper
  pureBin $ vlower < vupper && mot == Similar
    && abs bvi `mod` 12 == 7 && abs uwvi > 2

fHiddenOctave :: Voice v => v -> v -> Feature v
fHiddenOctave vlower vupper = do
  mot  <- aMotion vlower vupper
  bvi  <- aBVI vlower vupper
  uwvi <- aWVI vupper
  pureBin $ vlower < vupper && mot == Similar
    && bvi /= 0 && abs bvi `mod` 12 == 0 && abs uwvi > 2

fNoDoubling :: Voice v => v -> v -> Feature v
fNoDoubling v1 v2 = do
  bvi <- aBVI v1 v2
  pureBin $ bvi `mod` 12 /= 0

fLTDoubling :: Voice v => v -> v -> Feature v
fLTDoubling v1 v2 = do
  bvi <- aBVI v1 v2
  p   <- aPitchI v1
  key <- aKey
  pureBin $ (bvi `mod` 12 == 0) && (p `mod` 12 == leadingTone key)

fForeignDoubling :: Voice v => v -> v -> Feature v
fForeignDoubling v1 v2 = do
  bvi <- aBVI v1 v2
  p   <- aPitchI v1
  key <- aKey
  pureBin $ (bvi `mod` 12 == 0) && (not $ (p `mod` 12) `elem` scale key)

fRootDoubling :: Voice v => v -> v -> Feature v
fRootDoubling v1 v2 = do
  bvi  <- aBVI v1 v2
  (root,_) <- aRootI
  p    <- aPitchI v1
  pureBin $ (bvi `mod` 12 == 0) && (p `mod` 12 == root)

f5thDoubling :: Voice v => v -> v -> Feature v
f5thDoubling v1 v2 = do
  bvi  <- aBVI v1 v2
  (root,_) <- aRootI
  p    <- aPitchI v1
  pureBin $ (bvi `mod` 12 == 0) && ((p - root) `mod` 12 == 7)

f3rdDoubling :: Voice v => v -> v -> Feature v
f3rdDoubling v1 v2 = do
  bvi  <- aBVI v1 v2
  (root,_) <- aRootI
  p    <- aPitchI v1
  let relp = (p - root) `mod` 12
  pureBin $ (bvi `mod` 12 == 0) && (relp == 3 || relp == 4)

fTensionDoubling :: Voice v => v -> v -> Feature v
fTensionDoubling v1 v2 = do
  bvi  <- aBVI v1 v2
  (root,_) <- aRootI
  p    <- aPitchI v1
  let relp = (p - root) `mod` 12
  pureBin $ (bvi `mod` 12 == 0) && not (relp `elem` [0,3,4,7])

fChord :: Feature v
fChord = snd <$> aRootI

-- TODO doublingPref, chords, position,

fSkipThenStep :: Voice v => v -> Feature v
fSkipThenStep v = do
  pwvi <- abs <$> aPrevWVI v
  wvi  <- abs <$> aWVI v
  new  <- not <$> aHolds v
  pureBin $ new && 2 < pwvi && pwvi <= 4 && wvi <= 2

fSkipThenArpg :: Voice v => v -> Feature v
fSkipThenArpg v = do
  pwvi <- aPrevWVI v
  wvi  <- aWVI v
  new  <- not <$> aHolds v
  pureBin $ new && ( pwvi == 4 && (wvi == 3 || wvi == 5) ||
                     pwvi == 3 && (wvi == 4 || wvi == 5) ||
                     pwvi == -5 && (wvi == -3 || wvi == -5) ||
                     pwvi == -3 && (wvi == -4 || wvi == -5) )

fSkipThenElse v = do
  pwvi <- abs <$> aPrevWVI v
  wvi  <- abs <$> aWVI v
  arpg <- fSkipThenArpg v
  new  <- not <$> aHolds v
  pureBin $ new && 2 < pwvi && pwvi <= 4 && wvi > 2 && arpg == 0

fSLeapThenSimStep v = do
  pwvi <- aPrevWVI v
  wvi  <- aWVI v
  new  <- not <$> aHolds v
  pureBin $
    4 < pwvi && pwvi <= 7 && 0 < wvi && wvi <= 2 ||
    -4 >= pwvi && pwvi >= -7 && 0 > wvi && wvi >= -2

fSLeapThenConStep v = do
  pwvi <- aPrevWVI v
  wvi  <- aWVI v
  new  <- not <$> aHolds v
  pureBin $ new && ( 4 < pwvi && pwvi <= 7 && 0 > wvi && wvi >= -2 ||
                     -4 >= pwvi && pwvi >= -7 && 0 < wvi && wvi <= 2 )

fSLeapThenElse v = do
  pwvi <- abs <$> aPrevWVI v
  wvi  <- abs <$> aWVI v
  new  <- not <$> aHolds v
  pureBin $ new && 4 < pwvi && pwvi <= 7 && wvi > 2

fLLeapThenConStep v = do
  pwvi <- aPrevWVI v
  wvi  <- aWVI v
  new  <- not <$> aHolds v
  pureBin $ new && ( pwvi > 7 && 0 > wvi && wvi >= -2 ||
                     pwvi < -7 && 0 < wvi && wvi <= 2 )

fLLeapThenElse v = do
  pwvi  <- abs <$> aPrevWVI v
  lltcs <- fLLeapThenConStep v
  new   <- not <$> aHolds v 
  pureBin $ new && pwvi > 7 && lltcs == 0

fDissLeap v = do
  wvi <- abs <$> aWVI v
  pureBin $
    (wvi `mod` 12) `elem` [6,10,11] ||
    wvi > 12 && (wvi `mod` 12) `elem` [1,2]

fRelStationary v1 v2 = do
  mot <- aMotion v1 v2
  pureBin $ mot == Stationary

fRelOblique v1 v2 = do
  mot <- aMotion v1 v2
  pureBin $ mot == Oblique

fRelParallel v1 v2 = do
  mot <- aMotion v1 v2
  pureBin $ mot == Parallel

fRelSimilar v1 v2 = do
  mot <- aMotion v1 v2
  pureBin $ mot == Similar

fRelContrary v1 v2 = do
  mot <- aMotion v1 v2
  pureBin $ mot == Contrary

-- not in original list, just for testing
fForeign :: Voice v => v -> Feature v
fForeign v = do
  pitch <- aPitchI v
  key <- aKey
  pureBin $ not $ (pitch `mod` 12) `elem` scale key

fForeignLT :: Voice v => v -> Feature v
fForeignLT v = do
  pitch <- aPitchI v
  key <- aKey
  let pc = pitch `mod` 12
  pureBin $ (not $ pc `elem` scale key) && (pc == leadingTone key)

fMelodyHolds :: Voice v => v -> Feature v
fMelodyHolds v = do
  bin <$> aHolds v

-- running features

runFeature :: AspectMem v -> Feature v -> AutoEnv v -> (Double, AspectMem v)
runFeature mem feat = (L.over L._1 $ maybe 0 id) . runAspect mem feat

listFeature :: [Feature v] -> AutoEnv v -> [Double]
listFeature feats env = map fst . tail $ scanl run (undefined,emptyMem) feats
  where run (_,mem) feat = runFeature mem feat env

runFeatureOn :: Voice v => Piece v -> Feature v -> [Double]
runFeatureOn piece feat = runOnPiece piece (fst . runFeature emptyMem feat)

runFeaturesOn :: Voice v => Piece v -> [Feature v] -> [[Double]]
runFeaturesOn piece feats = runOnPiece piece (listFeature feats)

-- default features

choralVPairs :: [(ChoralVoice,ChoralVoice)]
choralVPairs = [ (Bass,Tenor), (Bass,Alto), (Bass,Soprano)
               , (Tenor,Alto), (Tenor,Soprano), (Alto,Soprano)]

voicePairs = [(Bass, Tenor), (Bass, Alto), (Bass, Soprano),
              (Tenor, Bass), (Tenor, Alto), (Tenor, Soprano),
              (Alto, Bass), (Alto, Tenor), (Alto, Soprano),
              (Soprano, Bass), (Soprano, Tenor), (Soprano, Alto)]

voicePairsU = [(Bass,Tenor), (Bass, Alto), (Bass, Soprano),
              (Tenor, Alto), (Tenor, Soprano), (Alto, Soprano)]

data NamedFeature v = NamedFeature
                      { nfFeature :: Feature v
                      , nfName :: T.Text
                      , nfVoices :: [v]
                      }

name :: Feature v -> T.Text -> [v] -> NamedFeature v
name = NamedFeature

name1 :: Show v => (v -> Feature v) -> T.Text -> v -> NamedFeature v
name1 f n v = name (f v) (n <> " " <> T.pack (show v)) [v]

name2 :: Show v => (v -> v -> Feature v) -> T.Text -> v -> v -> NamedFeature v
name2 f n v1 v2 = name (f v1 v2)
                  (n <> " " <> T.pack (show v1) <> " " <> T.pack (show v2))
                  [v1, v2]

nOver :: Show v => (v -> Feature v) -> T.Text -> [v] -> [NamedFeature v]
nOver f n vs = map (name1 f n) vs

nOver2 :: Show v => (v -> v -> Feature v) -> T.Text -> [(v,v)] -> [NamedFeature v]
nOver2 f n vs = map (uncurry $ name2 f n) vs

testFeaturesNamed :: [NamedFeature ChoralVoice]
testFeaturesNamed = [name1 fMelodyStays "fMelodyStays" Soprano,
                     name1 fMelodyStays "fMelodyStays" Alto]

defaultFeaturesNamed =
  nOver fOutOfRange "fOutOfRange" voiceList ++
  [name fTooFarSA "fTooFarSA" [Soprano, Alto], name fTooFarAT "fTooFarAT" [Alto, Tenor]] ++
  nOver2 fUnison "fUnison" voicePairsU ++ 
  nOver fCommonTone "fCommonTone" voiceList ++
  nOver fNearestTone "fNearestTone" voiceList ++
  nOver fMelodyHolds "fMelodyHolds" voiceList ++ -- added
  nOver fMelodyStays "fMelodyStays" voiceList ++
  nOver fMelodyStep "fMelodyStep" voiceList ++
  nOver fMelodySkip "fMelodySkip" voiceList ++
  nOver fMelodyLeap "fMelodyLeap" voiceList ++
  nOver fMelodyOverleap "fMelodyOverleap" voiceList ++
  nOver2 fCrossing "fCrossing" voicePairsU ++
  nOver2 fOverlap "fOverlap" voicePairsU ++
  nOver2 fParUnison "fParUnison" voicePairsU ++
  nOver2 fParFifth "fParFifth" voicePairsU ++
  nOver2 fParOctave "fParOctave" voicePairsU ++
  nOver2 fConsFifth "fConsFifth" voicePairsU ++
  nOver2 fConsOctave "fConsOctave" voicePairsU ++
  nOver2 fHiddenFifth "fHiddenFifth" voicePairsU ++
  nOver2 fHiddenOctave "fHiddenOctave" voicePairsU ++
  nOver fForeign "fForeign" voiceList ++ -- added
  nOver fForeignLT "fForeignLT" voiceList ++ -- added
  nOver fSkipThenStep "fSkipThenStep" voiceList ++
  nOver fSkipThenArpg "fSkipThenArpg" voiceList ++
  nOver fSkipThenElse "fSkipThenElse" voiceList ++
  nOver fSLeapThenSimStep "fSLeapThenSimStep" voiceList ++
  nOver fSLeapThenConStep "fSLeapThenConStep" voiceList ++
  nOver fSLeapThenElse "fSLeapThenElse" voiceList ++
  nOver fLLeapThenConStep "fLLeapThenConStep" voiceList ++
  nOver fLLeapThenElse "fLLeapThenElse" voiceList ++
  nOver fDissLeap "fDissLeap" voiceList ++
  nOver2 fRelStationary "fRelStationary" voicePairsU ++
  nOver2 fRelOblique "fRelOblique" voicePairsU ++
  nOver2 fRelParallel "fRelParallel" voicePairsU ++
  nOver2 fRelSimilar "fRelSimilar" voicePairsU ++
  nOver2 fRelContrary "fRelContrary" voicePairsU ++
  nOver2 fNoDoubling "fNoDoubling" voicePairsU ++
  nOver2 fLTDoubling "fLTDoubling" voicePairsU ++
  nOver2 fForeignDoubling "fForeignDoubling" voicePairsU ++
  nOver2 fRootDoubling "fRootDoubling" voicePairsU ++
  nOver2 f5thDoubling "f5thDoubling" voicePairsU ++
  nOver2 f3rdDoubling "f3rdDoubling" voicePairsU ++
  nOver2 fTensionDoubling "fTensionDoubling" voicePairsU ++
  [name fChord "fChord" [Soprano, Alto, Tenor, Bass]]


defaultFeatures :: [Feature ChoralVoice]
defaultFeatures = map nfFeature defaultFeaturesNamed

defaultFeatureNames :: [T.Text]
defaultFeatureNames = map nfName defaultFeaturesNamed
