{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module VoiceLeading.Inference where

import           VoiceLeading.Base
import           VoiceLeading.Helpers
import           VoiceLeading.Automaton
import           VoiceLeading.Features
import           VoiceLeading.Distribution
import           VoiceLeading.Learning

import           System.Random.MWC              ( createSystemRandom
                                                , GenIO
                                                )
import           System.Random.MWC.Distributions
                                                ( uniformShuffle )
import           Control.Monad                  ( replicateM
                                                , mapM
                                                )
import           Data.Foldable                  ( foldlM )
import qualified Data.Vector                   as V
import qualified Data.Vector.Unboxed           as VU
import qualified Data.Vector.Generic           as VG
import qualified Data.Map.Strict               as M
import qualified Data.List                     as L
import           Data.Bifunctor                 ( second )
-- import Data.Hashable (Hashable, hash)

import           Text.Printf                    ( printf )
import           System.IO                      ( stdout
                                                , hFlush
                                                )

import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import qualified Control.Monad.Trans.State     as ST

-- randomization of pieces
--------------------------

uniformRandomEvent :: Voice v => GenIO -> IO (M.Map v Pitch)
uniformRandomEvent gen = do
  pairs <- mapM roll voiceList
  pure $ M.fromList pairs
 where
  roll :: v -> IO (v, Pitch)
  roll v = (,) v <$> chooseRandom pitchList gen

n2time :: Int -> (Int, Integer)
n2time n | n `mod` 4 == 1 = (0, 4)
         | n `mod` 4 == 2 = (3, 4)
         | otherwise      = (negate (n + 1) `mod` 3, 3)

uniformRandomPiece :: Voice v => Int -> GenIO -> IO (Piece v)
uniformRandomPiece n gen = do
  let
    (firstBeat, bpb) = n2time n
    meta = PieceMeta "Randomly Generated Piece" (bpb, 4) (KeySig 0 0)
    beats = map fromIntegral
      $ iterate (\b -> (b + 1) `mod` bpb) (fromIntegral firstBeat)
  evs <- replicateM n (uniformRandomEvent gen)
  pure $ Piece meta (zipWith toEv evs beats)

randomizePiece :: forall  v . Voice v => GenIO -> [v] -> Piece v -> IO (Piece v)
randomizePiece gen fixedVoices (Piece meta events) = Piece meta <$> newEvs
 where
  initState = M.fromList $ zip voiceList (repeat Rest)
  voices    = voiceList L.\\ fixedVoices
  newEvs    = do
    (evs, _) <- flip ST.runStateT initState $ mapM randomizeEvent events
    pure evs
  randomizeEvent :: Event v -> ST.StateT (M.Map v Pitch) IO (Event v)
  randomizeEvent (Event pitches b) = do
    prev     <- ST.get
    pitches' <- foldlM (randomizeNote prev) pitches voices
    pure $ Event pitches' b
  randomizeNote prev pitches v = do
    let prevp = prev M.! v
    p' <- liftIO $ chooseRandom (holdPitch prevp : pitchList) gen
    pure $ M.insert v p' pitches

-- MAP inference by simulated annealing
---------------------------------------

-- parameters: iteration, potential, temp, dC, dS
type InfLogger m = Int -> Double -> Double -> Double -> Double -> m ()

-- see doi:10.1016/j.physleta.2003.08.070
estimateGibbsTSA
  :: (Voice v, MonadIO m)
  => g
  -> AutoOpts v
  -> (  [EEvent v]
     -> M.Map v (V.Vector (Feature v), VU.Vector Int)
     -> ModelParams
     -> Double
     -> g
     -> m [EEvent v]
     )
  -> Piece v
  -> Model v
  -> Double
  -> Double
  -> Double
  -> Int
  -> Int
  -> InfLogger m
  -> m (Piece v)
estimateGibbsTSA gen opts step piece model t0 tEnd kA minIter maxDec logger =
  do
    logger 1 pot0 t0 0 0
    estMAP <- goEstimate 1 evs0 t0 pot0 0 0 evs0 pot0 0
    pure $ extractPiece meta estMAP
 where
  evs0    = extendPiece piece
  meta    = pieceMeta piece
  pot0    = evalPieceUnnormLog opts piece model
  featMap = featureMap voiceList (V.toList $ modelFeatures model)
  params  = modelParams model
  goEstimate k evs temp pot dC dS evsBest potBest nDec
    | k > minIter && temp < tEnd = pure evsBest
    | otherwise = do
      liftIO $ putStr "sampling ..."
      liftIO $ hFlush stdout
      evs' <- step evs featMap params (1 / temp) gen
      liftIO $ putStrLn " done"
      liftIO $ hFlush stdout
      let piece'   = extractPiece meta evs' -- slightly inefficent conversion
          pot'     = evalPieceUnnormLog opts piece' model
          dPot     = pot - pot' -- = - (pot' - pot) = - ((- cost') - (- cost)) = cost' - cost
          dC'      = dC + dPot
          dS'      = if dPot > 0 then dS - (dPot / temp) else dS
          temp'    = if dC' >= 0 || dS' == 0 then t0 else kA * (dC' / dS')
          nDec'    = if pot' <= potBest then nDec + 1 else 0
          evsBest' = if pot' > potBest then evs else evsBest
          potBest' = if pot' > potBest then pot' else potBest
      logger (k + 1) pot' temp' dC' dS'
      if nDec' > maxDec
        then goEstimate (k + 1) evsBest temp' pot' dC' dS' evsBest potBest 0
        else goEstimate (k + 1) evs' temp' pot' dC' dS' evsBest' potBest' nDec'

estimateGibbsNotes
  :: (Voice v)
  => GenIO
  -> AutoOpts v
  -> [v]
  -> Piece v
  -> Model v
  -> Double
  -> Double
  -> Double
  -> Int
  -> Int
  -> InfLogger IO
  -> IO (Piece v)
estimateGibbsNotes gen opts fixedVoices piece = estimateGibbsTSA gen
                                                                 opts
                                                                 step
                                                                 piece
 where
  ctx  = mkDefaultCtx opts (keySignature $ pieceMeta piece)
  pVec = V.fromList pitchList
  vVec = V.fromList (voiceList L.\\ fixedVoices)
  step = gibbsStepNote (const (pVec, ())) () vVec ctx

estimateGibbsNotes'
  :: (Voice v)
  => GenIO
  -> AutoOpts v
  -> [v]
  -> [V.Vector Pitch]
  -> Piece v
  -> Model v
  -> Double
  -> Double
  -> Double
  -> Int
  -> Int
  -> InfLogger IO
  -> IO (Piece v)
estimateGibbsNotes' gen opts fixedVoices groups piece = estimateGibbsTSA
  gen
  opts
  step
  piece
 where
  ctx  = mkDefaultCtx opts (keySignature $ pieceMeta piece)
  vVec = V.fromList (voiceList L.\\ fixedVoices)
  fpVec []       = error "Not enough pitch groups provided"
  fpVec (g : gs) = (g, gs)
  step = gibbsStepNote fpVec groups vVec ctx

-- local maximization
---------------------

-- TODO: this is broken, sometimes decreases score

maxNotewise
  :: Voice v
  => AutoOpts v
  -> Maybe (Piece v)
  -> Model v
  -> Int
  -> [v]
  -> IO (Piece v)
maxNotewise opts piece model len fixedVoices = do
  gen       <- createSystemRandom
  initPiece <- case piece of
    Nothing -> uniformRandomPiece len gen
    Just p  -> pure p
  let events   = extendPiece initPiece
      meta     = pieceMeta initPiece
      ctx      = mkDefaultCtx opts (keySignature meta)
      pVec     = V.fromList pitchList
      vVec     = V.fromList (voiceList L.\\ fixedVoices)
      params   = modelParams model
      indexMap = featureMap voiceList (V.toList $ modelFeatures model)
      featMap  = M.map (second $ VG.map (params VG.!)) indexMap
      estimateGo ePiece = do
        putStr "Maximizing: "
        hFlush stdout
        (changed, newEvs) <- runOverPiece ePiece firstState False []
        putStrLn $ printf
          "meanLogPotential = % 7.5f"
          (meanLogPotential opts (extractPiece meta newEvs) model)
        if changed then estimateGo newEvs else pure $ extractPiece meta ePiece
       where
        runOverPiece [] _ changed evAcc = pure (changed, reverse evAcc)
        runOverPiece (ev : evs) state changed evAcc = do
          (newEv, newSt) <- maxNote state ev evs ctx featMap gen pVec vVec
          if newEv /= ev
            then runOverPiece evs newSt True (newEv : evAcc)
            else runOverPiece evs newSt changed (newEv : evAcc)
  estimateGo events

maxNote
  :: Voice v
  => State v
  -> EEvent v
  -> [EEvent v]
  -> Context v
  -> M.Map v (V.Vector (Feature v), ModelParams)
  -> GenIO
  -> V.Vector Pitch
  -> V.Vector v
  -> IO (EEvent v, State v)
maxNote state orig evs ctx model gen pVec vVec = do
  voices <- uniformShuffle vVec gen
  ev     <- V.foldM' sampleNote (extract orig) voices
  pure $! (extendLike' ev orig, nextState state ev)
 where
    -- sampleNote :: Event v -> v -> Event v
  sampleNote event voice = do
    qualities <- V.mapM propose evVec
    pure $ evVec V.! V.maxIndex qualities
   where
    feats   = fst $ model M.! voice
    params  = snd $ model M.! voice
    -- prepare successor events
    lka     = lookahead evs voice -- lookahead
    sectext = take lka evs -- section of extended events to look at
    sect    = map extract sectext -- extracted sect
    -- prepare proposal events
    emap    = evMap event -- voice-pitch-map of current event
    mkEv p = event { evMap = M.insert voice p emap } -- replace the pitch of the current voice
    evVec' = V.map mkEv pVec -- try all possible pitches
    prevp  = lGet (M.findWithDefault [] voice (sPrevPitch state)) 0 -- previous pitch
    evVec  = case prevp of -- if preceded by pitch, add holding variant
      Just (Pitch i _) -> evVec' `V.snoc` mkEv (Pitch i True)
      _                -> evVec'
    propose ev = do
      counts <- sumFeaturesM feats fs
      pure $ evalModelUnnormLog counts params
     where
      sect'  = scanl1 (normalizeTiesScanner False) (ev : sect) -- correct ties
      sectx' = zipWith extendLike sect' (orig : sectext) -- also in extended section
      states = scanl nextState state (safeInit sect')
      fs     = runFeaturesOnEEvs sectx' states ctx feats
