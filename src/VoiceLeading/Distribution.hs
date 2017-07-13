{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module VoiceLeading.Distribution where

import VoiceLeading.Base
import VoiceLeading.Automaton
import VoiceLeading.Helpers (lGet, replaceHead, safeInit)
import VoiceLeading.IO.Midi -- TODO: remove when finished
import VoiceLeading.IO.LilyPond -- TODO: remove

import Data.List (transpose, tails)
import qualified Data.Vector as Vec
import qualified Data.Map.Strict as M
import Data.Bits (shiftR)
import System.Random.MWC (GenIO, createSystemRandom, uniform)
import System.Random.MWC.Distributions (normal, categorical, uniformShuffle)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad (foldM, replicateM)
import GHC.Base ((<|>), when, liftM)

import System.Mem (performGC)

import Debug.Trace as DT
import System.ProgressBar

-----------------
-- basic types --
-----------------

type ModelParams = [Double]

type FeatureCounts = [Double]

data Model v = Model { modelFeatures :: [Feature v]
                     , modelParams :: ModelParams }

----------------
-- evaluation --
----------------

-- countTrue :: [Bool] -> Double
-- countTrue = fromIntegral . length . filter id

countFeatures :: Voice v => Piece v -> [Feature v] -> FeatureCounts
countFeatures piece feats = map sum trans
  where trans = transpose $ runFeaturesOn piece feats
        
evalModelUnnorm :: FeatureCounts -> ModelParams -> Double
evalModelUnnorm fs thetas
  -- | if any (>0) fs then DT.trace ("positive features: " ++ show fs) False else False = undefined
  | length fs == length thetas = exp $ sum (zipWith (*) fs thetas)
  | otherwise = error $ "feature counts and params vectors must have the same length:\
                        \|fs| = " ++ show (length fs) ++ ", |thetas| = " ++ show (length thetas)

evalModel :: FeatureCounts -> ModelParams -> Double -> Double
evalModel fs thetas z = evalModelUnnorm fs thetas / z

--------------------
-- Gibbs sampling --
--------------------

runOnEEvs :: Voice v => [EEvent v] -> [State v] -> Context v -> (AutoEnv v -> a) -> [a]
runOnEEvs evs sts ctx scanner = map scanner (zipWith3 AutoEnv evs sts (repeat ctx))

runFeaturesOnEEvs :: Voice v => [EEvent v] -> [State v] -> Context v -> [Feature v] -> [[Double]]
runFeaturesOnEEvs evs sts ctx feats = runOnEEvs evs sts ctx listRunner
  where runners        = map runFeature feats
        listRunner env = map ($ env) runners

-- event-wise
-------------

eventVector :: forall v . Voice v => Vec.Vector (Event v)
eventVector = Vec.fromList eventList

gibbsStepEv :: Voice v =>
  [EEvent v] -> Context v -> Model v -> GenIO -> IO [EEvent v]
gibbsStepEv evs ctx model gen = do
  -- putStrLn $ "gibbsStep: evs = " ++ show (take 1 (tails evs))
  newEvs <- evScanGo (return (undefined, firstState)) (init $ tails evs)
  pure $ tail newEvs
  where evVec = eventVector
        sampleEv state evs = evKernel state evs ctx model gen evVec
        evScanGo acc evss = do
          -- performGC
          -- putStrLn $ "evScanner: " ++ show (take 10 evs)
          (accev, accst) <- acc
          rest <- (case evss of
                     []  -> pure []
                     evs:revss -> evScanGo (sampleEv accst evs) revss)
          pure $ accev : rest

lookaheadV :: Voice v => [EEvent v] -> v -> Int
lookaheadV evs v = laGo evs v 1 1
  where laGo [] _ _ num = num
        laGo (ev:evs) voice moves num
          | moves >= memoryLength = num
          | otherwise =  laGo evs voice (incV moves) (num+1)
          where ev' = extract ev
                incV i = if pitchHolds (evGet ev' voice) then i else i+1

lookahead :: forall v . Voice v => [EEvent v] -> Int
lookahead [] = 0
lookahead (_:evs) = let vl  = voiceList :: [v]
                in maximum $ map (lookaheadV evs) vl

extendLike' :: Event v -> EEvent v -> EEvent v
extendLike' (Event m _) (EEvent _ b f l) = EEvent m b f l

evKernel :: Voice v =>
  State v -> [EEvent v] -> Context v -> Model v ->
  GenIO -> Vec.Vector (Event v) -> IO (EEvent v, State v)
evKernel state (orig:evs) ctx model gen evVec = do
  putStrLn $ "evKernel: " ++ show (extract orig)
  putStrLn $ "lookahead: " ++ show lka
  (prog,thread) <- startProgress exact percentage 40 2560000
  q <- qualities prog
  putStrLn "\nsampling from proposal distribution"
  i <- categorical q gen
  putStrLn "done"
  let ev = evVec Vec.! i
  pure $! (extendLike ev orig, nextState state ev)
  where
    lka     = lookahead evs
    section = take lka evs
    sectevs = map extract section
    feats   = modelFeatures model
    propose prog ev i = do
      --propose ev = do
      when (i `mod` 80000 == 0) (incProgress prog 80000)
      pure $! evalModelUnnorm counts (modelParams model)
      --evalModelUnnorm counts (modelParams model)
        where states = scanl nextState state (ev : init sectevs)
              eev    = extendLike' ev orig
              fs     = runFeaturesOnEEvs (eev:section) states ctx feats
              counts = map sum (transpose fs)
    qualities prog = Vec.zipWithM (propose prog) evVec (Vec.generate (Vec.length evVec) id)
    --q = Vec.map propose evVec

gibbsEvPiece1 :: Piece ChoralVoice -> IO (Piece ChoralVoice)
gibbsEvPiece1 p@(Piece meta _) = do
  gen <- createSystemRandom
  p1 <- normal 0 1 gen
  p2 <- normal 0 1 gen
  let model = Model testFeatures [p1, p2]
  newEvs <- gibbsStepEv evs ctx model gen
  return $ Piece meta (map extract newEvs)
  where ctx = mkDefaultCtx (keySignature meta)
        evs = extendPiece p

gibbsEv1 :: Piece ChoralVoice -> IO (EEvent ChoralVoice, State ChoralVoice)
gibbsEv1 piece = do
  gen <- createSystemRandom
  p1 <- normal 0 1 gen
  p2 <- normal 0 1 gen
  let model = Model testFeatures [p1, p2]
      evVec = eventVector
  evKernel firstState evs ctx model gen evVec
  where ctx = mkDefaultCtx (keySignature (pieceMeta piece))
        evs = extendPiece piece

-- note-wise
------------

pitchVector :: Vec.Vector Pitch
pitchVector = Vec.fromList pitchList

gibbsStepNote :: Voice v =>
  [EEvent v] -> Context v -> Model v -> GenIO -> IO [EEvent v]
gibbsStepNote evs ctx model gen = do
  newEvs <- evScanGo (return (undefined, firstState)) (init $ tails evs)
  pure $ tail newEvs
  where pVec = pitchVector
        vVec = Vec.fromList voiceList
        sampleEv state evs = noteKernel state evs ctx model gen pVec vVec
        evScanGo acc evss = do
          (accev, accst) <- acc
          rest <- (case evss of
                     [] -> pure []
                     evs:revss -> evScanGo (sampleEv accst evs) revss)
          pure $ accev : rest

noteKernel :: forall v . Voice v =>
  State v -> [EEvent v] -> Context v -> Model v ->
  GenIO -> Vec.Vector Pitch -> Vec.Vector v -> IO (EEvent v, State v)
noteKernel state (orig:evs) ctx model gen pVec vVec = do
  voices <- Vec.toList <$> uniformShuffle vVec gen
  ev <- foldM sampleNote (extract orig) voices
  pure $! (extendLike ev orig, nextState state ev)
  where
    feats = modelFeatures model
    sampleNote :: Event v -> v -> IO  (Event v)
    sampleNote event voice = do
      i <- categorical qualities gen
      pure $! evVec Vec.! i
      where
        -- prepare successor events
        lka     = lookaheadV evs voice -- lookahead
        sectext = take lka evs -- section of extended events to look at
        sect    = map extract sectext -- extracted sect
        -- prepare proposal events
        emap    = evMap event -- voice-pitch-map of current event
        mkEv p  = event { evMap = M.insert voice p emap} -- replace the pitch of the current voice
        evVec'  = Vec.map mkEv pVec -- try all possible pitches
        prevp   = lGet (M.findWithDefault [] voice (sPrevPitch state)) 0 -- previous pitch
        evVec   = case prevp of -- if preceded by pitch, add holding variant
                    Just (Pitch i _) -> evVec' `Vec.snoc` (mkEv $ Pitch i True)
                    _ -> evVec'
        propose ev = evalModelUnnorm counts (modelParams model)
          where sect'  = scanl1 (normalizeTiesScanner False) (ev : sect) -- correct ties
                sectx' = zipWith extendLike sect' (orig:sectext) -- also in extended section
                states = scanl nextState state (safeInit sect')
                fs     = runFeaturesOnEEvs sectx' states ctx feats
                counts = map sum (transpose fs)
        qualities = Vec.map propose evVec

gibbsNotePiece1 :: Piece ChoralVoice -> IO (Piece ChoralVoice)
gibbsNotePiece1 p@(Piece meta _) = do
  gen <- createSystemRandom
  params <- replicateM (length feats) (normal 0 10 gen)
  putStrLn $ "params: " ++ show params
  let model = Model feats params
  newEvs <- gibbsStepNote evs ctx model gen
  return $ Piece meta (map extract newEvs)
  where ctx = mkDefaultCtx (keySignature meta)
        evs = extendPiece p
        feats = defaultFeatures

gibbsNote1 :: Piece ChoralVoice -> IO (EEvent ChoralVoice, State ChoralVoice)
gibbsNote1 piece = do
  gen <- createSystemRandom
  p1 <- normal 0 1 gen
  p2 <- normal 0 1 gen
  let model = Model testFeatures [p1, p2]
      pVec = pitchVector
      vVec = Vec.fromList voiceList
  noteKernel firstState evs ctx model gen pVec vVec
  where ctx = mkDefaultCtx (keySignature (pieceMeta piece))
        evs = extendPiece piece
