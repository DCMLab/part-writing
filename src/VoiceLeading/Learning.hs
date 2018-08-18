{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module VoiceLeading.Learning where

import VoiceLeading.Base
import VoiceLeading.Automaton
import VoiceLeading.Distribution
import VoiceLeading.Helpers (lGet, replaceHead, safeInit, iterateM)

import Data.List (transpose, tails, foldl1')
import qualified Data.Vector as Vec
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import System.Random.MWC (GenIO, createSystemRandom, uniform)
import System.Random.MWC.Distributions (normal, categorical, uniformShuffle)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad (foldM, replicateM, zipWithM, when)
import Control.Monad.IO.Class (MonadIO(..))

import Control.DeepSeq (deepseq)
import System.IO (hFlush, stdout)
import System.Mem (performGC)

import Debug.Trace as DT
import System.ProgressBar
import Data.Hashable (Hashable, hash)

--------------------
-- Gibbs sampling --
--------------------

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
  (prog,thread) <- startProgress exact percentage 40 (Progress 0 2560000)
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
    feats   = map nfFeature $ modelFeatures model
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
  let model = Model testFeaturesNamed [p1, p2]
  newEvs <- gibbsStepEv evs ctx model gen
  return $ Piece meta (map extract newEvs)
  where ctx = mkDefaultCtx (keySignature meta)
        evs = extendPiece p

gibbsEv1 :: Piece ChoralVoice -> IO (EEvent ChoralVoice, State ChoralVoice)
gibbsEv1 piece = do
  gen <- createSystemRandom
  p1 <- normal 0 1 gen
  p2 <- normal 0 1 gen
  let model = Model testFeaturesNamed [p1, p2]
      evVec = eventVector
  evKernel firstState evs ctx model gen evVec
  where ctx = mkDefaultCtx (keySignature (pieceMeta piece))
        evs = extendPiece piece

-- note-wise
------------

pitchVector :: Vec.Vector Pitch
pitchVector = Vec.fromList pitchList

gibbsStepNote :: forall v . (Hashable v, Voice v) =>
  [EEvent v] -> Context v -> Model v -> Double -> GenIO ->
  Vec.Vector Pitch -> Vec.Vector v -> IO [EEvent v]
gibbsStepNote evs ctx model power gen pVec vVec = do
  --putStr $ "gibbsStep: oldHash = " ++ show (hash evs)
  newEvs <- evScanGo (return (undefined, firstState)) (init $ tails evs)
  rand <- uniform gen :: IO Double
  --putStr $ ", rand = " ++ show rand
  --putStrLn $ ", newHash = " ++ show (hash $ tail newEvs)
  pure $ tail newEvs
  where featpar = zip (modelFeatures model) (modelParams model)
        featv v = (v, unzip $ map (\(f,p) -> (nfFeature f, p)) $
                      filter (\(f,_) -> v `elem` nfVoices f) featpar)
        feats = M.fromList $ map featv voiceList
        sampleEv state es = noteKernel state es ctx feats power gen pVec vVec
        evScanGo acc evss = do
          (accev, accst) <- acc
          rest <- (case evss of
                     [] -> pure []
                     es:revss -> evScanGo (sampleEv accst es) revss)
          pure $ accev : rest

gibbsStepNote' :: (Hashable v, Voice v) =>
  [EEvent v] -> Context v -> Model v -> Double -> GenIO -> IO [EEvent v]
gibbsStepNote' evs ctx model power gen = do
  gibbsStepNote evs ctx model power gen pVec vVec
  where pVec = pitchVector
        vVec = Vec.fromList voiceList

noteKernel :: forall v . Voice v =>
  State v -> [EEvent v] -> Context v -> M.Map v ([Feature v], [Double]) ->
  Double -> GenIO -> Vec.Vector Pitch -> Vec.Vector v -> IO (EEvent v, State v)
noteKernel state (orig:evs) ctx model power gen pVec vVec = do
  voices <- Vec.toList <$> uniformShuffle vVec gen
  ev <- foldM sampleNote (extract orig) voices
  pure $! (extendLike ev orig, nextState state ev)
  where
    sampleNote :: Event v -> v -> IO  (Event v)
    sampleNote event voice = do
      i <- categorical qualities gen
      pure $! evVec Vec.! i
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
        propose ev = evalModelUnnorm counts params ** power
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
  newEvs <- gibbsStepNote' evs ctx model 1.0 gen
  return $ Piece meta (map extract newEvs)
  where ctx = mkDefaultCtx (keySignature meta)
        evs = extendPiece p
        feats = defaultFeaturesNamed

gibbsNote1 :: Piece ChoralVoice -> IO (EEvent ChoralVoice, State ChoralVoice)
gibbsNote1 piece = do
  gen <- createSystemRandom
  p1 <- normal 0 1 gen
  p2 <- normal 0 1 gen
  let model = Model testFeaturesNamed [p1, p2]
      pVec = pitchVector
      vVec = Vec.fromList voiceList
      featpar = zip (modelFeatures model) (modelParams model)
      featv v = (v, unzip $ map (\(f,p) -> (nfFeature f, p)) $
                    filter (\(f,_) -> v `elem` nfVoices f) featpar)
      feats = M.fromList $ map featv voiceList
  noteKernel firstState evs ctx feats 1.0 gen pVec vVec
  where ctx = mkDefaultCtx (keySignature (pieceMeta piece))
        evs = extendPiece piece

--------------
-- training --
--------------

-- expectedFeats :: Voice v => [Piece v] -> [Feature v] -> [Double]
-- expectedFeats pieces features = map (/n) counts
--   where n      = fromIntegral $ sum (map pieceLen pieces)
--         counts = foldr1 (zipWith (+)) (map ((flip countFeatures) features) pieces)

-- | Scale down a list of parameters such that the largest absolute value is 1.
compress :: [Double] -> ([Double],Double)
compress params = (map (/m) params, m)
  where m = 1 -- maximum $ map abs params

data TrainingLogEntry = TLogEntry
                        { leIt :: Int
                        , leProgress :: Double
                        , leNames :: [T.Text]
                        , leParams :: [Double]
                        , leGradient :: [Double]
                        , lePower :: Double
                        , leRate :: Double }

trainPCD :: (Hashable v, Voice v, MonadIO m)
         => [Piece v] -> [NamedFeature v] -> Int -> Int -> Int
         -> (Double -> Double) -> (Double -> Double) -> (TrainingLogEntry -> m ())
         -> m (Model v, [Piece v])
trainPCD pieces features iterations chainSize restartEvery fPower fRate logger = do
  gen <- liftIO createSystemRandom
  liftIO $ putStr "counting features of corups... "
  liftIO $ hFlush stdout
  let feats   = map nfFeature features
      fNames  = map nfName features
  expData <- pure $! expectedFeats pieces feats -- force before "done." message
  liftIO $ expData `deepseq` putStrLn "done."
  initChain <- liftIO $ take chainSize . Vec.toList <$> uniformShuffle (Vec.fromList pieces) gen
  let initParams = replicate (length features) 0 -- alternative: normal 0 1 gen?
      chainMetas = map pieceMeta initChain
      chainCtxs  = map (mkDefaultCtx . keySignature) chainMetas
      pVec       = pitchVector
      vVec       = Vec.fromList voiceList
      iterd      = fromIntegral iterations - 1
      update (params, chain, it) = do
        let progress = fromIntegral it / iterd
            power    = fPower progress
        chain' <- liftIO $ if (restartEvery > 0 && it `mod` restartEvery == 0)
                           then zipWithM (sampleZipper 0) chain chainCtxs
                           else pure chain
        newChain <- zipWithM (sampleZipper power) chain' chainCtxs
        let newChain'       = zipWith extractPiece chainMetas newChain
            expChain        = expectedFeats newChain' feats
            gradient        = zipWith (-) expData expChain
            rate            = fRate progress
            newParams       = zipWith (\p g -> p+rate*g) params gradient
        logger $ TLogEntry it progress fNames newParams gradient power rate
        pure (newParams, newChain, it+1)
        where model = Model features params
              sampleZipper pw p c = liftIO $ gibbsStepNote p c model pw gen pVec vVec
  liftIO $ putStrLn $ "chain info: " ++ show chainMetas
  (finalParams, finalChain, _) <-
    (iterateM iterations update (initParams, map extendPiece initChain, 0))
  let chainPieces = zipWith extractPiece chainMetas finalChain 
  pure $ (Model features finalParams, chainPieces)
