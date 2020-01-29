{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module VoiceLeading.Learning where

import           VoiceLeading.Base
import           VoiceLeading.Automaton
import           VoiceLeading.Features
import           VoiceLeading.Distribution
import           VoiceLeading.Helpers           ( lGet
                                                , safeInit
                                                , iterateM
                                                , chooseRandom
                                                )

import           Data.List                      ( scanl' )
import qualified Data.Vector                   as V
import qualified Data.Vector.Unboxed           as VU
import qualified Data.Map.Strict               as M
import           System.Random.MWC              ( GenIO
                                                , Gen
                                                )
import           System.Random.MWC.Distributions
                                                ( normal
                                                , categorical
                                                , uniformShuffle
                                                , bernoulli
                                                )
import           Control.Monad.Primitive        ( PrimMonad
                                                , PrimState
                                                )
import           Control.Monad                  ( foldM
                                                , zipWithM
                                                )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Bifunctor                 ( first
                                                , second
                                                )
import qualified Streamly                      as S
import qualified Streamly.Prelude              as S

import           MonadUtils                     ( mapAccumLM )

--------------------
-- Gibbs sampling --
--------------------

-- event-wise
-------------

lookahead :: Voice v => [EEvent v] -> v -> Int
lookahead events v = laGo events v 1 1
 where
  laGo [] _ _ num = num
  laGo (ev : evs) voice moves num
    | moves >= memoryLength = num
    | otherwise             = laGo evs voice (incV moves) (num + 1)
   where
    ev' = extract ev
    incV i = if pitchHolds (evGet ev' voice) then i else i + 1

extendLike' :: Event v -> EEvent v -> EEvent v
extendLike' (Event m _) (EEvent _ b f l) = EEvent m b f l

-- note-wise
------------

pitchVector :: V.Vector Pitch
pitchVector = V.fromList pitchList

featureMap
  :: (Voice v)
  => [v]
  -> [NamedFeature v]
  -> M.Map v (V.Vector (Feature v), VU.Vector Int)
featureMap voices features = M.fromList $ voiceFeats <$> voices
 where
  featInds = V.fromList $ zip features [0 ..]
  voiceFeats v =
    ( v
    , second VU.convert
      $   V.unzip
      $   first nfFeature
      <$> V.filter ((v `elem`) . nfVoices . fst) featInds
    )


gibbsStepNote
  :: (Voice v)
  => (s -> (V.Vector Pitch, s))
  -> s
  -> V.Vector v
  -> Context v
  -> [EEvent v]
  -> M.Map v (V.Vector (Feature v), VU.Vector Int)
  -> ModelParams
  -> Double
  -> GenIO
  -> IO [EEvent v]
gibbsStepNote fpVec fpInit vVec ctx events featMap params power gen = evScanGo
  []
  firstState
  events
  fpInit
 where
  feats = M.map (second $ VU.map (params VU.!)) featMap
  sampleEv !state !ev !evs !pVec =
    noteKernel state ev evs ctx feats power gen pVec vVec
  evScanGo acc  _   []         _        = pure $ reverse acc
  evScanGo !acc !st (ev : evs) !fpState = do
    let (pVec, fpSt') = fpVec fpState
    (ev', st') <- sampleEv st ev evs pVec
    evScanGo (ev' : acc) st' evs fpSt'

noteKernel
  :: forall v
   . Voice v
  => State v
  -> EEvent v
  -> [EEvent v]
  -> Context v
  -> M.Map v (V.Vector (Feature v), VU.Vector Double)
  -> Double
  -> GenIO
  -> V.Vector Pitch
  -> V.Vector v
  -> IO (EEvent v, State v)
noteKernel state orig evs ctx model power gen pVec vVec = do
  voices <- V.toList <$> uniformShuffle vVec gen
  ev     <- foldM sampleNote (extract orig) voices
  pure (extendLike ev orig, nextState state ev)
 where
  sampleNote :: Event v -> v -> IO (Event v)
  sampleNote event voice = do
    qualities <- V.fromList <$> S.toList qualStr
    i         <- categorical qualities gen
    pure $! evVec V.! i
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
      pure $ evalModelUnnorm counts params ** power
     where
      sect'  = scanl1 (normalizeTiesScanner False) (ev : sect) -- correct ties
      sectx' = zipWith extendLike sect' (orig : sectext) -- also in extended section
      states = scanl' nextState state (safeInit sect')
      fs     = runFeaturesOnEEvs sectx' states ctx feats
    qualStr = S.aheadly $ S.mapM propose $ S.fromFoldable $ V.force evVec

--------------
-- training --
--------------

data TrainingLogEntry = TLogEntry
                        { leIt :: Int
                        , leProgress :: Double
                        , leParams :: ModelParams
                        , leGradient :: ModelParams
                        , lePower :: Double
                        , leRate :: Double
                        , leChainData :: FeatureCounts
                        }

trainPCD
  :: (Voice v, S.MonadAsync m, Optimize o)
  => o
  -> GenIO
  -> AutoOpts v
  -> [Piece v]
  -> FeatureCounts
  -> [NamedFeature v]
  -> Int
  -> Int
  -> Int
  -> (Double -> Double)
  -> (Double -> Double)
  -> (Double -> Double)
  -> (TrainingLogEntry -> m ())
  -> m (Model v, [Piece v])
trainPCD optimizer gen opts pieces expData features iterations chainSize restartEvery fPower fRate fRateFast logger
  = do
    let feats   = V.fromList $ nfFeature <$> features
        nFeats  = V.length feats
        featMap = featureMap voiceList features
    initChain <-
      liftIO
      $   take chainSize
      .   V.toList
      <$> uniformShuffle (V.fromList pieces) gen
    -- let initChain = take chainSize pieces
    initParams <- VU.replicateM nFeats $ liftIO $ normal 0 0.1 gen
    -- let initParams = VU.replicate nFeats 0 -- alternative: normal 0 1 gen?
    let
      initFastParams = VU.replicate nFeats 0 -- as per fast PCD
      chainMetas     = map pieceMeta initChain
      chainCtxs      = map (mkDefaultCtx opts . keySignature) chainMetas
      pVec           = pitchVector
      vVec           = voiceVector
      iterd          = fromIntegral iterations - 1
      update (params, fastParams, chain, it, opt) = do
        let progress = fromIntegral it / iterd
            power    = fPower progress
        chain' <- liftIO $ if restartEvery > 0 && it `mod` restartEvery == 0
          then zipWithM (sampleZipper 0) chain chainCtxs
          else pure chain
        newChain <- zipWithM (sampleZipper power) chain' chainCtxs
        let
          newChain'          = zipWith extractPiece chainMetas newChain
          expChain           = expectedFeats opts newChain' feats
          rawGradient        = VU.zipWith (-) expData expChain
          rate               = fRate progress
          (newOpt, gradient) = optimize opt progress rate rawGradient
          rateFast           = fRateFast progress
          newParams          = VU.zipWith (+) params gradient
          newFastParams      = VU.zipWith
            (\p g -> p * 0.95 + (rateFast / rate) * g)
            fastParams
            gradient
        logger $ TLogEntry it progress newParams gradient power rate expChain
        pure (newParams, newFastParams, newChain, it + 1, newOpt)
       where
        paramsCombined = VU.zipWith (+) params fastParams
        sampleZipper pw p c = liftIO $ gibbsStepNote (const (pVec, ()))
                                                     ()
                                                     vVec
                                                     c
                                                     p
                                                     featMap
                                                     paramsCombined
                                                     pw
                                                     gen
    liftIO $ putStrLn $ "chain info: " ++ show chainMetas
    (finalParams, _, finalChain, _, _) <- iterateM
      iterations
      update
      (initParams, initFastParams, map extendPiece initChain, 0, optimizer)
    let chainPieces = zipWith extractPiece chainMetas finalChain
    pure (Model (V.fromList features) finalParams, chainPieces)

----------------
-- optimizers --
----------------

-- type Optimizer a = a -> Double -> Double -> ModelParams -> (a, ModelParams)

class Optimize a where
  optimize :: a -> Double -> Double -> ModelParams -> (a, ModelParams)

-- no optimization
------------------

instance Optimize () where
  optimize _ _ rate gradient = ((), VU.map (* rate) gradient)

-- momentum
-----------

data Momentum = Momentum (Double -> Double) ModelParams

instance Optimize Momentum where
  optimize (Momentum fCoeff expAvg) progress rate gradient =
    (Momentum fCoeff expAvg', expAvg')
   where
    coeff = fCoeff progress
    mom avg grd = coeff * avg + rate * grd
    expAvg' = VU.zipWith mom expAvg gradient

momentum :: (Double -> Double) -> Int -> Momentum
momentum f n = Momentum f $ VU.replicate n 0

-- adam
-------

data Adam = Adam Double Double Double ModelParams ModelParams

epsilon :: Double
-- epsilon = 2 ** (-53)
epsilon = 10 ** (-8)

instance Optimize Adam where
  optimize (Adam b1 b2 t ms vs) _ rate gradient =
    (Adam b1 b2 (t + 1) ms' vs', gradient')
   where
    ms'       = VU.zipWith (\m g -> (b1 * m) + ((1 - b1) * g)) ms gradient
    vs'       = VU.zipWith (\v g -> (b2 * v) + ((1 - b2) * g * g)) vs gradient
    gradient' = VU.zipWith updt ms' vs'
    updt m v = (rate / (sqrt vc + epsilon)) * mc
     where
      mc = m / (1 - b1 ** t)
      vc = v / (1 - b2 ** t)

adam :: Double -> Double -> Int -> Adam
adam b1 b2 n = Adam b1 b2 1 zeros zeros where zeros = VU.replicate n 0

-----------------------
-- stopping criteria --
-----------------------

neighbor
  :: forall v m
   . (Voice v, PrimMonad m)
  => Gen (PrimState m)
  -> Double
  -> Piece v
  -> m (Piece v)
neighbor gen k (Piece meta evs) =
  Piece meta . snd <$> mapAccumLM neighborEv emptyEvent evs
 where
  coin :: m Bool
  coin = bernoulli k gen
  neighborEv :: Event v -> Event v -> m (Event v, Event v)
  neighborEv prev ev = do
    ev' <- foldM (sampleVoice prev) ev voiceList
    pure (ev', ev')
  sampleVoice :: Event v -> Event v -> v -> m (Event v)
  sampleVoice prev ev@(Event pitches beat) v = do
    change <- coin
    if change
      then (\p' -> Event (M.insert v p' pitches) beat) <$> drawPitch
      else pure ev
   where
    prevp :: Maybe Pitch
    prevp = evGetMaybe prev v
    pLst  = case prevp of -- if preceded by pitch, add holding variant
      Just (Pitch i _) -> Pitch i True : pitchList
      _                -> pitchList
    drawPitch = chooseRandom pLst gen

stoppingXi :: FeatureCounts -> [FeatureCounts] -> ModelParams -> Double
stoppingXi train neighbors params = xiTrain - xiNb
 where
  xiTrain = evalModelUnnormLog train params
  xiNb = log $ sum $ (\nb -> exp $ evalModelUnnormLog nb params) <$> neighbors
