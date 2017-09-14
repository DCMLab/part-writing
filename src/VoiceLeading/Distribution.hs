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
module VoiceLeading.Distribution (
  -- * Types
  Model(..), ModelParams, FeatureCounts,
  -- * Evaluation
  countFeatures,
  evalModelUnnormLog, evalModelUnnorm, evalModel,
  -- ** on whole pieces
  evalPieceUnnormLog, evalPieceUnnorm,
  -- ** statistics
  meanLogPotential, meanLogPotentialN,
  meanFeatCounts, meanFeatCountsN,
  expectedFeats
  ) where

import VoiceLeading.Base
import VoiceLeading.Automaton

import Data.List (transpose)
import Data.Text (unpack)

-----------------
-- basic types --
-----------------

-- | Type for a list of model paramters.
--   Each parameter should correspond to a feature in the 'Model' being used
--   and stands for the weight of that feature.
type ModelParams = [Double]

-- | Type for a list of feature counts.
--   Each value should correspond to a feature in the 'Model' being used
--   and stands for the sum over all events in the piece/corpus of that feature.
type FeatureCounts = [Double]

-- | A 'Model' characterizes a probability/quality distribution over pieces.
--   The quality of a piece is determined using features
--   (each of which assigns a value to each event) and weights for these features
--   (i.e., the models parameters).
--   Features in a 'Model' are given as 'NamedFeature's (which wrap 'Feature's with
--   some metadata) in order to represent all necessary information about the model.
data Model v = Model { modelFeatures :: [NamedFeature v]
                     , modelParams :: ModelParams }

instance Show v => Show (Model v) where
  show (Model fs ps) = "Model:\n" ++ concat (zipWith showF fs ps)
    where showF (NamedFeature _ n v) p = (unpack n) ++ " " ++ show p ++ "\n"

----------------
-- evaluation --
----------------

-- | Runs a list of 'Feature's over a 'Piece' and sums up the results.
countFeatures :: Voice v => Piece v -> [Feature v] -> FeatureCounts
countFeatures piece feats = map sum trans
  where trans = transpose $ runFeaturesOn piece feats
        -- runFeaturesOn returns events as rows and features as colums,
        -- transpose turns it into features as rows and events as colums.
        -- sum then runs runs over each row, i.e., sums per feature.

-- | Takes a list of feature values (counted from some 'Piece')
--   and corresponding weights, and returns the overall log quality of that piece.
evalModelUnnormLog :: FeatureCounts -> ModelParams -> Double
evalModelUnnormLog fs thetas
  | length fs == length thetas = sum (zipWith (*) fs thetas)
  | otherwise = error $ "feature counts and params vectors must have the same length:\
                        \|fs| = " ++ show (length fs) ++ ", |thetas| = " ++ show (length thetas)

-- | Like 'evalModelUnnormLog' but returns the full (non-log) quality.
--   The result is scaled by a factor that corresponds to maximum of the model parameters
--   since the exponentiation step can lead to 'Infinity' for large parameters.
--   Also, the result does not include the (intractable) partition function,
--   so it is not a normalized probability.
--   Therefore, the result is only comparable accross the same model parameters.
evalModelUnnorm :: FeatureCounts -> ModelParams -> Double
evalModelUnnorm fs thetas = exp $ evalModelUnnormLog fs thetas - m
  where m = maximum thetas

-- | Like 'evalModelUnnorm', but allows providing a partition factor.
--   Not really usable , just for completeness.
evalModel :: FeatureCounts -> ModelParams -> Double -> Double
evalModel fs thetas z = evalModelUnnorm fs thetas / z

-- | Evaluates a whole 'Piece' by counting the features and combining them
--   To the full quality value of the piece. Like with 'evalModelUnnorm'
--   the value is unnormalized scaled to avoid 'Infinity'
--   and only comparable for the same model parameters.
evalPieceUnnorm :: Voice v => Piece v -> Model v -> Double
evalPieceUnnorm piece (Model nfeats params) =
  evalModelUnnorm (countFeatures piece (unname nfeats)) params

-- | Evaluates the log quality of a whole 'Piece' under a given 'Model'.
evalPieceUnnormLog :: Voice v => Piece v -> Model v -> Double
evalPieceUnnormLog piece (Model nfeats params) =
  evalModelUnnormLog (countFeatures piece (unname nfeats)) params

-- | Returns the average log potential (= local quality) per event.
meanLogPotential :: Voice v => Piece v -> Model v -> Double
meanLogPotential piece model =
  evalPieceUnnormLog piece model / fromIntegral (pieceLen piece)

-- | like 'meanLogPotential' but for several pieces.
meanLogPotentialN :: Voice v => [Piece v] -> Model v -> Double
meanLogPotentialN ps model =
  let nevs   = sum $ map pieceLen ps
      logpot = sum $ map (flip evalPieceUnnormLog model) ps in
    logpot / fromIntegral nevs

-- | Returns for each feature in the given 'Model'
--   the average value of that feature per event in the given 'Piece'.
meanFeatCounts :: Voice v => Piece v -> Model v -> [Double]
meanFeatCounts piece (Model nfeats _) =
  map (/ fromIntegral (pieceLen piece)) (countFeatures piece (unname nfeats))

-- | Returns for each feature in the given 'Model'
--   the average value of that feature per event in the given 'Piece's.
--   Like 'meanFeatCounts' but for several pieces.
meanFeatCountsN :: Voice v => [Piece v] -> Model v -> [Double]
meanFeatCountsN ps (Model nfeats _) = expectedFeats ps (unname nfeats)

-- | Returns for each given feature the average (= "expected")
--   value of that feature per event in the given 'Piece's.
--   Like 'meanFeatCountsN', but features are not taken from a 'Model'.
expectedFeats :: Voice v => [Piece v] -> [Feature v] -> [Double]
expectedFeats pieces features = map (/n) counts
  where n      = fromIntegral $ sum (map pieceLen pieces)
        counts = foldr1 (zipWith (+)) (map ((flip countFeatures) features) pieces)

-- helper function, extracts features from named features
unname :: [NamedFeature v] -> [Feature v]
unname nfs = map nfFeature nfs
