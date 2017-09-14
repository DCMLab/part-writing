-- {-# LANGUAGE ScopedTypeVariables #-}
module VoiceLeading.Inference where

import VoiceLeading.Base
import VoiceLeading.Helpers
import VoiceLeading.Automaton ( EEvent(..), State(..), Context(..)
                              , firstState, nextState, mkDefaultCtx
                              , extendPiece, extendLike, extractPiece, extract
                              , NamedFeature(..), Feature(..), runFeaturesOnEEvs)
import VoiceLeading.Distribution ( Model(..), meanLogPotential
                                 , evalModelUnnormLog, evalPieceUnnormLog)
import VoiceLeading.Learning ( lookaheadV, lookahead, eventVector, extendLike'
                             , gibbsStepNote)

import System.Random.MWC (createSystemRandom, uniformR, GenIO)
import System.Random.MWC.Distributions (uniformShuffle)
import Control.Monad (replicateM, mapM, when)
import Data.Foldable (foldlM)
import qualified Data.Vector as Vec
import qualified Data.Map.Strict as M
import Data.List (transpose, (\\))
import Data.Hashable (Hashable, hash)

import System.ProgressBar
import Text.Printf (printf)
import System.IO (stdout, hFlush)

import VoiceLeading.IO.LilyPond

uniformRandomEvent :: Voice v => GenIO -> IO (M.Map v Pitch)
uniformRandomEvent gen = do
  pairs <- mapM roll voiceList
  pure $ M.fromList pairs
  where roll :: Voice v => v -> IO (v, Pitch)
        roll v = (,) v <$> chooseRandom pitchList gen
  
n2time :: Int -> (Int, Integer)
n2time n
  | n `mod` 4 == 1 = (0, 4)
  | n `mod` 4 == 2 = (3, 4)
  | otherwise      = ((0-n+1) `mod` 3, 3)

uniformRandomPiece :: Voice v => Int -> GenIO -> IO (Piece v)
uniformRandomPiece n gen = do
  let (firstBeat, bpb) = n2time n
      meta = PieceMeta "Randomly Generated Piece" (bpb, 4) (KeySig 0 0)
      beats = map fromIntegral $ iterate (\b -> (b+1) `mod` bpb) (fromIntegral firstBeat)
  evs <- replicateM n (uniformRandomEvent gen)
  pure $ Piece meta (zipWith Event evs beats)

uniformRandomPiece' :: Voice v => Int -> IO (Piece v)
uniformRandomPiece' n = do
  gen <- createSystemRandom
  uniformRandomPiece n gen

mapEstimateEventwise :: Voice v => Model v -> Int -> IO (Piece v)
mapEstimateEventwise model len = do
  init <- uniformRandomPiece' len
  let evs   = extendPiece init
      meta  = pieceMeta init
      ctx   = mkDefaultCtx (keySignature meta)
      evVec = eventVector
      estimateGo ePiece = do
        putStrLn "run over piece"
        (changed, newEvs) <- runOverPiece ePiece firstState False []
        if changed then estimateGo newEvs else pure $ extractPiece meta ePiece
          where runOverPiece [] _ changed evAcc = pure (changed, reverse evAcc)
                runOverPiece (ev:evs) state changed evAcc = do
                  (newEv, newSt) <- maxEvent model ev evs state ctx evVec
                  if newEv /= ev
                    then runOverPiece evs newSt True (newEv:evAcc)
                    else runOverPiece evs newSt changed (newEv:evAcc)
  estimateGo evs

maxEvent :: Voice v =>
            Model v -> EEvent v -> [EEvent v] -> State v -> Context v -> Vec.Vector (Event v)
         -> IO (EEvent v, State v)
maxEvent (Model nfeats params) orig evs state ctx evVec = do
  putStrLn $ "maximizing event, lka = " ++ show lka
  (prog, thread) <- startProgress exact percentage 40 2560000
  vals <- Vec.zipWithM (evalEv prog) evVec (Vec.generate (Vec.length evVec) id)
  let new = evVec Vec.! Vec.maxIndex vals
  pure $! (extendLike' new orig, nextState state new)
  where lka     = lookahead evs
        section = take lka evs
        sectevs = map extract section
        feats   = map nfFeature nfeats
        evalEv prog new i = do
          when (i `mod` 8000 == 0) (incProgress prog 8000)
          pure $! evalModelUnnormLog counts params
            where states = scanl nextState state (new : init sectevs)
                  enew   = extendLike' new orig
                  fs     = runFeaturesOnEEvs (enew:section) states ctx feats
                  counts = map sum (transpose fs)

mapEstimateNotewise :: Voice v => Maybe (Piece v) -> Model v -> Int -> [v] -> IO (Piece v)
mapEstimateNotewise piece model len fixedVoices = do
  gen <- createSystemRandom
  init <- case piece of
            Nothing -> uniformRandomPiece len gen
            Just p  -> pure p
  let evs   = extendPiece init
      meta  = pieceMeta init
      ctx   = mkDefaultCtx (keySignature meta)
      pVec = Vec.fromList pitchList
      vVec = Vec.fromList (voiceList \\ fixedVoices)
      featpar = zip (modelFeatures model) (modelParams model)
      featv v = (v, unzip $ map (\(f,p) -> (nfFeature f, p)) $
                    filter (\(f,_) -> v `elem` nfVoices f) featpar)
      feats = M.fromList $ map featv voiceList
      estimateGo ePiece = do
        putStr "Maximizing: "
        hFlush stdout
        (changed, newEvs) <- runOverPiece ePiece firstState False []
        putStrLn $ printf "meanLogPotential = % 7.5f"
          (meanLogPotential (extractPiece meta newEvs) model)
        if changed then estimateGo newEvs else pure $ extractPiece meta ePiece
          where runOverPiece [] _ changed evAcc = pure (changed, reverse evAcc)
                runOverPiece (ev:evs) state changed evAcc = do
                  (newEv, newSt) <- maxNote state (ev:evs) ctx feats gen pVec vVec
                  if newEv /= ev
                    then runOverPiece evs newSt True (newEv:evAcc)
                    else runOverPiece evs newSt changed (newEv:evAcc)
  estimateGo evs

maxNote :: Voice v =>
  State v -> [EEvent v] -> Context v -> M.Map v ([Feature v], [Double]) ->
  GenIO -> Vec.Vector Pitch -> Vec.Vector v -> IO (EEvent v, State v)
maxNote state (orig:evs) ctx model gen pVec vVec = do
  voices <- uniformShuffle vVec gen
  let ev = Vec.foldl sampleNote (extract orig) voices
  pure $! (extendLike' ev orig, nextState state ev)
  where
    -- sampleNote :: Event v -> v -> Event v
    sampleNote event voice =
      evVec Vec.! index
      where
        feats   = fst $ model M.! voice
        params  = snd $ model M.! voice
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
        propose ev = evalModelUnnormLog counts params
          where sect'  = scanl1 (normalizeTiesScanner False) (ev : sect) -- correct ties
                sectx' = zipWith extendLike sect' (orig:sectext) -- also in extended section
                states = scanl nextState state (safeInit sect')
                fs     = runFeaturesOnEEvs sectx' states ctx feats
                counts = map sum (transpose fs)
        qualities = Vec.map propose evVec
        index = Vec.maxIndex qualities

bestEstimate :: Voice v => IO [(Piece v)] -> Model v ->  IO (Piece v, Double)
bestEstimate estimates model = do
  (p1:ps) <- estimates
  pure $ foldl comp (p1, evalPieceUnnormLog p1 model) ps 
  where comp (old, oldScore) new =
          let newScore = evalPieceUnnormLog new model in
            if newScore > oldScore
            then (new, newScore)
            else (old, oldScore)

estimateGibbsAnnealing :: (Hashable v, Voice v) => Piece v -> Model v -> [v] -> Int -> (Double -> Double)
  -> IO (Piece v)
estimateGibbsAnnealing piece model fixedVoices iterations fPower = do
  gen <- createSystemRandom
  let evs     = extendPiece piece
      meta    = pieceMeta piece
      ctx     = mkDefaultCtx (keySignature meta)
      pVec    = Vec.fromList pitchList
      vVec    = Vec.fromList (voiceList \\ fixedVoices)
      -- featpar = zip (modelFeatures model) (modelParams model)
      -- featv v = (v, unzip $ map (\(f,p) -> (nfFeature f, p)) $
      --               filter (\(f,_) -> v `elem` nfVoices f) featpar)
      -- feats   = M.fromList $ map featv voiceList
      iterd   = fromIntegral iterations - 1
      -- power n = (targetPower * (fromIntegral n / iterd))
      -- goEstimate :: Int -> [EEvent v] -> IO [EEvent v]
      goEstimate n evs
        | n >= iterations = pure evs
        | otherwise       = do
            --putStrLn $ "Sampling: n = " ++ show n ++ ", power = " ++ show power
            printf "Sampling: n = %d, power = % 5.2f" n power
            hFlush stdout
            newEvs <- gibbsStepNote evs ctx model power gen pVec vVec
            let p = (extractPiece meta newEvs)
            -- putStrLn $ "piece hash = " ++ show (hash p) ++
            --   ", meanLogPot = " ++ show (meanLogPotential p model)
            putStrLn $ printf ", meanLogPot = % 7.5f" (meanLogPotential p model)
            goEstimate (n+1) newEvs
            where progress = fromIntegral n / iterd
                  power    = fPower progress
  est <- (goEstimate 0 evs)
  pure $ extractPiece meta est
