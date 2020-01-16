{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-|
Module      : VoiceLeading.Distribution
Description : Representing a distribution as features and parameters
Copyright   : (c) Christoph Finkensiep, 2017
License     : MIT
Maintainer  : chfin@chfin.de
Stability   : experimental
Portability : POSIX

The quality of a pieces wrt. to voice leading is can be formalized
as a probability distribution.
This probability distribution is characterized by features and feature weights.
Each feature assings a value to each state-event-pair in the piece
(cf. "VoiceLeading.Automaton" for details on features and states), which is multiplied
with the feature weight and then exponentiated.
The product of these values for all features on one state-event pair then if the
"(local) potential" of that pair.
The product of all local potentials then is the global potential:

\[ \phi(piece) = \prod_{(s,e) \in piece} \prod_i \exp \theta_i f_i(s,e)
= \exp \sum_{(s,e) \in piece} \sum_i \theta_i f_i(s,e)
= \exp \sum_{(s,e) \in piece} \vec{\theta_i} \vec{f_i}(s,e)
= \exp \vec{\theta_i} \sum_{(s,e) \in piece} \vec{f_i}(s,e)\]

If this is normalized to sum up to 1, we have a probability distribution:

\[ p(piece) = \dfrac{1}{Z} \phi(piece) \]

\[ Z = \sum_{piece \in pieces} \phi(piece) \]

This module provides types for representing distributions as models (cf. 'Model'),
and functions for evaluating the potential and different statistics of a piece.
-}
module VoiceLeading.Distribution
  (
  -- * Types
    Model(..)
  , ModelParams
  , FeatureCounts
  ,
  -- * Evaluation
    sumFeaturesP
  , countFeaturesP
  , sumFeaturesM
  , countFeaturesM
  , evalModelUnnormLog
  , evalModelUnnorm
  , evalModel
  ,
  -- ** on whole pieces
    evalPieceUnnormLog
  , evalPieceUnnorm
  ,
  -- ** statistics
    meanLogPotential
  , meanLogPotentialN
  , meanFeatCounts
  , meanFeatCountsN
  , expectedFeats
  , expectedFeatsM
  )
where

import           VoiceLeading.Base
import           VoiceLeading.Automaton

import           Data.List                      ( transpose )
import           Data.Text                      ( unpack )
import qualified Data.Vector                   as V
import qualified Data.Vector.Unboxed           as VU
import qualified Data.Vector.Unboxed.Mutable   as VUM
import           Data.Foldable                  ( foldl' )

import qualified Streamly                      as S
import qualified Streamly.Prelude              as S
import           Control.Monad.IO.Class         ( MonadIO )
import           Control.DeepSeq                ( force )
import qualified Control.Loop                  as L
import           Control.Monad                  ( forM_ )
import           Control.Monad.Primitive        ( PrimMonad )

-----------------
-- basic types --
-----------------

-- | Type for a list of model paramters.
--   Each parameter should correspond to a feature in the 'Model' being used
--   and stands for the weight of that feature.
type ModelParams = VU.Vector Double

-- | Type for a list of feature counts.
--   Each value should correspond to a feature in the 'Model' being used
--   and stands for the sum over all events in the piece/corpus of that feature.
type FeatureCounts = VU.Vector Double

-- | A 'Model' characterizes a probability/quality distribution over pieces.
--   The quality of a piece is determined using features
--   (each of which assigns a value to each event) and weights for these features
--   (i.e., the models parameters).
--   Features in a 'Model' are given as 'NamedFeature's (which wrap 'Feature's with
--   some metadata) in order to represent all necessary information about the model.
data Model v = Model { modelFeatures :: V.Vector (NamedFeature v)
                     , modelParams :: ModelParams }

instance Show v => Show (Model v) where
  show (Model fs ps) = "Model:\n" ++ concat (V.zipWith showF fs (V.convert ps))
    where showF (NamedFeature _ n v) p = unpack n <> " " <> show p <> "\n"

----------------
-- evaluation --
----------------

sumFeaturesP
  :: (Foldable t, Num a, VU.Unbox a)
  => V.Vector b
  -> t (VU.Vector a)
  -> VU.Vector a
sumFeaturesP !feats !counts =
  foldl' (VU.zipWith (+)) (VU.convert $ 0 <$ feats) counts

sumFeaturesM
  :: (Foldable t, Num a, VU.Unbox a, PrimMonad m)
  => V.Vector b
  -> t (VU.Vector a)
  -> m (VU.Vector a)
sumFeaturesM !feats !counts = do
  accum <- VUM.replicate n 0
  forM_ counts (addVec accum)
  VU.unsafeFreeze accum
 where
  n = V.length feats
  addVec !accum !vec = L.forLoop 0 (< n) (+ 1) $ addIndex accum vec
  addIndex accum vec i = VUM.modify accum (+ (vec VU.! i)) i

-- | Runs a list of 'Feature's over a 'Piece' and sums up the results.
countFeaturesP
  :: Voice v => AutoOpts v -> Piece v -> V.Vector (Feature v) -> FeatureCounts
countFeaturesP opts piece feats =
  sumFeaturesP feats $ runFeaturesOn opts piece feats

countFeaturesM
  :: (Voice v, PrimMonad m)
  => AutoOpts v
  -> Piece v
  -> V.Vector (Feature v)
  -> m FeatureCounts
countFeaturesM opts piece feats =
  sumFeaturesM feats $ runFeaturesOn opts piece feats

-- | Takes a list of feature values (counted from some 'Piece')
--   and corresponding weights, and returns the overall log quality of that piece.
evalModelUnnormLog :: FeatureCounts -> ModelParams -> Double
evalModelUnnormLog fs thetas
  | VU.length fs == VU.length thetas
  = VU.sum (VU.zipWith (*) fs thetas)
  | otherwise
  = error
    $  "feature counts and params vectors must have the same length: |fs| = "
    ++ show (VU.length fs)
    ++ ", |thetas| = "
    ++ show (VU.length thetas)

-- | Like 'evalModelUnnormLog' but returns the full (non-log) quality.
--   The result is scaled by a factor that corresponds to maximum of the model parameters
--   since the exponentiation step can lead to 'Infinity' for large parameters.
--   Also, the result does not include the (intractable) partition function,
--   so it is not a normalized probability.
--   Therefore, the result is only comparable accross the same model parameters.
evalModelUnnorm :: FeatureCounts -> ModelParams -> Double
evalModelUnnorm fs thetas = exp $ evalModelUnnormLog fs thetas - m
  where m = VU.maximum thetas

-- evalModelUnnormM :: (PrimMonad m) => FeatureCounts -> ModelParams -> m Double
-- evalModelUnnormM fs thetas = exp <$> evalModelUnnormLogM fs thetas - m
--   where m = VU.maximum thetas

-- | Like 'evalModelUnnorm', but allows providing a partition factor.
--   Not really usable , just for completeness.
evalModel :: FeatureCounts -> ModelParams -> Double -> Double
evalModel fs thetas z = evalModelUnnorm fs thetas / z

-- | Evaluates a whole 'Piece' by counting the features and combining them
--   To the full quality value of the piece. Like with 'evalModelUnnorm'
--   the value is unnormalized scaled to avoid 'Infinity'
--   and only comparable for the same model parameters.
evalPieceUnnorm :: Voice v => AutoOpts v -> Piece v -> Model v -> Double
evalPieceUnnorm opts piece (Model nfeats params) =
  evalModelUnnorm (countFeaturesP opts piece (unname nfeats)) params

-- | Evaluates the log quality of a whole 'Piece' under a given 'Model'.
evalPieceUnnormLog :: Voice v => AutoOpts v -> Piece v -> Model v -> Double
evalPieceUnnormLog opts piece (Model nfeats params) =
  evalModelUnnormLog (countFeaturesP opts piece (unname nfeats)) params

-- | Returns the average log potential (= local quality) per event.
meanLogPotential :: Voice v => AutoOpts v -> Piece v -> Model v -> Double
meanLogPotential opts piece model =
  evalPieceUnnormLog opts piece model / fromIntegral (pieceLen piece)

-- | like 'meanLogPotential' but for several pieces.
meanLogPotentialN :: Voice v => AutoOpts v -> [Piece v] -> Model v -> Double
meanLogPotentialN opts ps model =
  let nevs   = sum $ map pieceLen ps
      logpot = sum $ map (flip (evalPieceUnnormLog opts) model) ps
  in  logpot / fromIntegral nevs

-- | Returns for each feature in the given 'Model'
--   the average value of that feature per event in the given 'Piece'.
meanFeatCounts :: Voice v => AutoOpts v -> Piece v -> Model v -> FeatureCounts
meanFeatCounts opts piece (Model nfeats _) = VU.map
  (/ fromIntegral (pieceLen piece))
  (countFeaturesP opts piece (unname nfeats))

-- | Returns for each feature in the given 'Model'
--   the average value of that feature per event in the given 'Piece's.
--   Like 'meanFeatCounts' but for several pieces.
meanFeatCountsN
  :: Voice v => AutoOpts v -> [Piece v] -> Model v -> FeatureCounts
meanFeatCountsN opts ps (Model nfeats _) =
  expectedFeats opts ps (unname nfeats)

-- | Returns for each given feature the average (= "expected")
--   value of that feature per event in the given 'Piece's.
--   Like 'meanFeatCountsN', but features are not taken from a 'Model'.
expectedFeats
  :: Voice v => AutoOpts v -> [Piece v] -> V.Vector (Feature v) -> FeatureCounts
expectedFeats opts pieces features = VU.map (/ n) counts
 where
  n           = fromIntegral $ sum (map pieceLen pieces)
  pieceCounts = flip (countFeaturesP opts) features <$> pieces
  counts      = sumFeaturesP features pieceCounts

expectedFeatsM
  :: (Voice v, S.MonadAsync m)
  => AutoOpts v
  -> [Piece v]
  -> V.Vector (Feature v)
  -> m FeatureCounts
expectedFeatsM !opts !pieces !features = do
  let pieceCounts =
        S.asyncly
          $ S.mapM (pure . flip (countFeaturesP opts) features)
          $ S.fromList
          $ force pieces
      init   = (VU.convert $ 0 <$ features)
      folder = VU.zipWith (+)
  counts <- S.foldl' folder init pieceCounts
  pure $ VU.map (/ n) counts
  where n = fromIntegral $ sum (map pieceLen pieces)


-- helper function, extracts features from named features
-- unname :: [NamedFeature v] -> [Feature v]
unname = fmap nfFeature
