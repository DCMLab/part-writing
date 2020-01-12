-- {-# LANGUAGE ScopedTypeVariables #-}
module VoiceLeading.Inference where

import           VoiceLeading.Base
import           VoiceLeading.Helpers
import           VoiceLeading.Automaton
import           VoiceLeading.Distribution
import           VoiceLeading.Learning

import           System.Random.MWC              ( createSystemRandom
                                                , uniformR
                                                , GenIO
                                                )
import           System.Random.MWC.Distributions
                                                ( uniformShuffle )
import           Control.Monad                  ( replicateM
                                                , mapM
                                                , when
                                                )
import           Data.Foldable                  ( foldlM )
import qualified Data.Vector                   as V
import qualified Data.Vector.Unboxed           as VU
import qualified Data.Vector.Generic           as VG
import qualified Data.Map.Strict               as M
import           Data.List                      ( transpose
                                                , (\\)
                                                )
import           Data.Bifunctor                 ( second )
-- import Data.Hashable (Hashable, hash)

import           System.ProgressBar
import           Text.Printf                    ( printf )
import           System.IO                      ( stdout
                                                , hFlush
                                                )

import           VoiceLeading.IO.LilyPond

uniformRandomEvent :: Voice v => GenIO -> IO (M.Map v Pitch)
uniformRandomEvent gen = do
  pairs <- mapM roll voiceList
  pure $ M.fromList pairs
 where
  roll :: Voice v => v -> IO (v, Pitch)
  roll v = (,) v <$> chooseRandom pitchList gen

n2time :: Int -> (Int, Integer)
n2time n | n `mod` 4 == 1 = (0, 4)
         | n `mod` 4 == 2 = (3, 4)
         | otherwise      = ((0 - n + 1) `mod` 3, 3)

uniformRandomPiece :: Voice v => Int -> GenIO -> IO (Piece v)
uniformRandomPiece n gen = do
  let
    (firstBeat, bpb) = n2time n
    meta = PieceMeta "Randomly Generated Piece" (bpb, 4) (KeySig 0 0)
    beats = map fromIntegral
      $ iterate (\b -> (b + 1) `mod` bpb) (fromIntegral firstBeat)
  evs <- replicateM n (uniformRandomEvent gen)
  pure $ Piece meta (zipWith Event evs beats)

uniformRandomPiece' :: Voice v => Int -> IO (Piece v)
uniformRandomPiece' n = do
  gen <- createSystemRandom
  uniformRandomPiece n gen

mapEstimateEventwise :: Voice v => AutoOpts v -> Model v -> Int -> IO (Piece v)
mapEstimateEventwise opts model len = do
  init <- uniformRandomPiece' len
  let evs   = extendPiece init
      meta  = pieceMeta init
      ctx   = mkDefaultCtx opts (keySignature meta)
      evVec = eventVector
      estimateGo ePiece = do
        putStrLn "run over piece"
        (changed, newEvs) <- runOverPiece ePiece firstState False []
        if changed then estimateGo newEvs else pure $ extractPiece meta ePiece
       where
        runOverPiece [] _ changed evAcc = pure (changed, reverse evAcc)
        runOverPiece (ev : evs) state changed evAcc = do
          (newEv, newSt) <- maxEvent model ev evs state ctx evVec
          if newEv /= ev
            then runOverPiece evs newSt True (newEv : evAcc)
            else runOverPiece evs newSt changed (newEv : evAcc)
  estimateGo evs

maxEvent
  :: Voice v
  => Model v
  -> EEvent v
  -> [EEvent v]
  -> State v
  -> Context v
  -> V.Vector (Event v)
  -> IO (EEvent v, State v)
maxEvent (Model nfeats params) orig evs state ctx evVec = do
  putStrLn $ "maximizing event, lka = " ++ show lka
  prog <- newProgressBar (defStyle { stylePrefix = exact })
                         10
                         (Progress 0 2560000 ())
  vals <- V.zipWithM (evalEv prog) evVec (V.generate (V.length evVec) id)
  let new = evVec V.! V.maxIndex vals
  pure $! (extendLike' new orig, nextState state new)
 where
  lka     = lookahead evs
  section = take lka evs
  sectevs = extract <$> section
  feats   = nfFeature <$> nfeats
  evalEv prog new i = do
    when (i `mod` 8000 == 0) (incProgress prog 8000)
    pure $! evalModelUnnormLog counts params
   where
    states = scanl nextState state (new : init sectevs)
    enew   = extendLike' new orig
    fs     = runFeaturesOnEEvs (enew : section) states ctx feats
    counts = sumFeaturesP feats fs

mapEstimateNotewise
  :: Voice v
  => AutoOpts v
  -> Maybe (Piece v)
  -> Model v
  -> Int
  -> [v]
  -> IO (Piece v)
mapEstimateNotewise opts piece model len fixedVoices = do
  gen  <- createSystemRandom
  init <- case piece of
    Nothing -> uniformRandomPiece len gen
    Just p  -> pure p
  let evs      = extendPiece init
      meta     = pieceMeta init
      ctx      = mkDefaultCtx opts (keySignature meta)
      pVec     = V.fromList pitchList
      vVec     = V.fromList (voiceList \\ fixedVoices)
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
          (newEv, newSt) <- maxNote state (ev : evs) ctx featMap gen pVec vVec
          if newEv /= ev
            then runOverPiece evs newSt True (newEv : evAcc)
            else runOverPiece evs newSt changed (newEv : evAcc)
  estimateGo evs

maxNote
  :: Voice v
  => State v
  -> [EEvent v]
  -> Context v
  -> M.Map v (V.Vector (Feature v), ModelParams)
  -> GenIO
  -> V.Vector Pitch
  -> V.Vector v
  -> IO (EEvent v, State v)
maxNote state (orig : evs) ctx model gen pVec vVec = do
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
    lka     = lookaheadV evs voice -- lookahead
    sectext = take lka evs -- section of extended events to look at
    sect    = map extract sectext -- extracted sect
    -- prepare proposal events
    emap    = evMap event -- voice-pitch-map of current event
    mkEv p = event { evMap = M.insert voice p emap } -- replace the pitch of the current voice
    evVec' = V.map mkEv pVec -- try all possible pitches
    prevp  = lGet (M.findWithDefault [] voice (sPrevPitch state)) 0 -- previous pitch
    evVec  = case prevp of -- if preceded by pitch, add holding variant
      Just (Pitch i _) -> evVec' `V.snoc` (mkEv $ Pitch i True)
      _                -> evVec'
    propose ev = do
      counts <- sumFeaturesM feats fs
      pure $ evalModelUnnormLog counts params
     where
      sect'  = scanl1 (normalizeTiesScanner False) (ev : sect) -- correct ties
      sectx' = zipWith extendLike sect' (orig : sectext) -- also in extended section
      states = scanl nextState state (safeInit sect')
      fs     = runFeaturesOnEEvs sectx' states ctx feats

bestEstimate
  :: Voice v => AutoOpts v -> IO [(Piece v)] -> Model v -> IO (Piece v, Double)
bestEstimate opts estimates model = do
  (p1 : ps) <- estimates
  pure $ foldl comp (p1, evalPieceUnnormLog opts p1 model) ps
 where
  comp (old, oldScore) new =
    let newScore = evalPieceUnnormLog opts new model
    in  if newScore > oldScore then (new, newScore) else (old, oldScore)

estimateGibbsAnnealing
  :: (Voice v)
  => AutoOpts v
  -> (  [EEvent v]
     -> M.Map v (V.Vector (Feature v), VU.Vector Int)
     -> ModelParams
     -> Double
     -> GenIO
     -> IO [EEvent v]
     )
  -> Piece v
  -> Model v
  -> Int
  -> (Double -> Double)
  -> IO (Piece v)
estimateGibbsAnnealing opts step piece model iterations fPower = do
  gen <- createSystemRandom
  let
    evs     = extendPiece piece
    meta    = pieceMeta piece
    iterd   = fromIntegral iterations - 1
    featMap = featureMap voiceList (V.toList $ modelFeatures model)
    -- goEstimate :: Int -> [EEvent v] -> IO [EEvent v]
    goEstimate n evs
      | n >= iterations = pure evs
      | otherwise = do
        printf "Sampling: n = %d, power = % 5.2f" n power
        hFlush stdout
        newEvs <- step evs featMap (modelParams model) power gen
        let p = extractPiece meta newEvs
        putStrLn
          $ printf ", meanLogPot = % 7.5f" (meanLogPotential opts p model)
        goEstimate (n + 1) newEvs
     where
      progress = fromIntegral n / iterd
      power    = fPower progress
  est <- (goEstimate 0 evs)
  pure $ extractPiece meta est

estimateGibbsNotes
  :: (Voice v)
  => AutoOpts v
  -> [v]
  -> Piece v
  -> Model v
  -> Int
  -> (Double -> Double)
  -> IO (Piece v)
estimateGibbsNotes opts fixedVoices piece = estimateGibbsAnnealing opts
                                                                   step
                                                                   piece
 where
  ctx  = mkDefaultCtx opts (keySignature $ pieceMeta piece)
  pVec = V.fromList pitchList
  vVec = V.fromList (voiceList \\ fixedVoices)
  step = gibbsStepNote (const (pVec, ())) () vVec ctx

estimateGibbsNotes'
  :: (Voice v)
  => AutoOpts v
  -> [v]
  -> [V.Vector Pitch]
  -> Piece v
  -> Model v
  -> Int
  -> (Double -> Double)
  -> IO (Piece v)
estimateGibbsNotes' opts fixedVoices groups piece = estimateGibbsAnnealing
  opts
  step
  piece
 where
  ctx  = mkDefaultCtx opts (keySignature $ pieceMeta piece)
  vVec = V.fromList (voiceList \\ fixedVoices)
  fpVec []       = error "Not enough pitch groups provided"
  fpVec (g : gs) = (g, gs)
  step = gibbsStepNote fpVec groups vVec ctx

-- estimateGibbsEvents opts kernel kinit piece = estimateGibbsAnnealing opts step piece
--   where ctx = mkDefaultCtx opts (keySignature $ pieceMeta piece)
--         step evs m p gen = gibbsStepEv kernel kinit evs ctx m p gen
