{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module: VoiceLeading.Prob
Description : Probabilistic voice leading rules
Copyright   : (c) Christoph Finkensiep, 2017
License     : MIT
Maintainer  : chfin@freenet.de
Stability   : experimental
Portability : POSIX

This module is fundamentally broken and awaits a rewrite!

Probabilistic voice leading rules can make judgements about a particular voice leading that are more finegrained than "allowed" and forbidden.
In general, a rule defines a measure μ for on voice leading events (μ(e))or pairs of events (μ(e_2|e_1)), which can be interpreted as the preferability of the event as such or the event in the context of its predecessor with regard to the rule.
Intuitively, a rule measure tells to which degree the event complies with the rule.

If a rule measure is normalized, it corresponds to a probability, which is again to be interpreted as peferability.
This way, a voice leading system can be viewed as a markov model describing the probability of an event given its predecessor: p(e_i|e_(i-1)).

A judgement of an event's voice leading quality can be given by combining the individual measures to a product of experts (PoE):
p(e_i|e_{i-1}) = 1/Z Π_j μ_j (e_i|e_(i-1)), where Z is a normalization factor.
In this context the individual measures are called experts.
Since the individual rules have independent models and do not share their parameters, they can be trained separately.

This module defines an 'Expert' typeclass as a general interface for training and applying experts.
The 'ProductOfExperts' type implements a product of 'Expert's as an instance of the 'Expert' typeclass, so that it can be used to train all of its experts simultaneously.
Experts (including PoEs) are not normalized by default, they are measures but not probability measures.
in order to get normalized results use 'judgeNorm' and 'judgeGivenNorm'.
-}
module VoiceLeading.Prob
  ( Expert(..)
  , ProductOfExperts(..)
  , TexturalDensity(..)
  , learn, learn1, learnAll, learnAll1
  ) where

import VoiceLeading.Base
import VoiceLeading.Load
import qualified Data.Vector.Unboxed as VU
import qualified Data.Sequence as S
-- import Data.Dynamic
import qualified Data.Bits as Bits

import qualified Debug.Trace as DT

--------- --
-- Expert --
------------

class Expert e where
  neutral    :: e
  judge      :: Voice v => e -> EEvent v -> Float
  judgeGiven :: Voice v => e -> EEvent v -> EEvent v -> Float
  learnNew1  :: Voice v => e -> EEvent v -> e
  learnNew   :: Voice v => e -> (EEvent v, EEvent v) -> e

  judge      _ _        = 1.0
  judgeGiven exp e2 _   = judge exp e2
  learnNew1  exp _      = exp
  learnNew   exp (_,e2) = learnNew1 exp e2

learn :: (Expert e, Voice v) => e -> EPiece v -> e
learn expert piece = foldl learnNew expert (zip (init piece) (tail piece))

learn1 :: (Expert e, Voice v) => EPiece v -> e
learn1 = learn neutral

learnAll :: (Expert e, Voice v) => e -> Pieces v -> e
learnAll expert pieces = foldl learn expert (map toEPiece pieces)

learnAll1 :: (Expert e, Voice v) => Pieces v -> e
learnAll1 = learnAll neutral

-- judgeNorm :: (Expert e, Voice v) => e -> EEvent v -> Float
-- judgeNorm expert event = (judge expert event) / z
--   where z = sum $ map (judge expert) eEventList

judgeGivenNorm :: (Expert e, Voice v) => e -> EEvent v -> EEvent v -> Float
judgeGivenNorm expert e2 e1 = (judgeGiven expert e2 e1) / z
  where z = sum $ map (\e -> judgeGiven expert e e1) eEventList

-- null expert type
-------------------

data NullExpert = NullExpert
  deriving (Show)

instance Expert NullExpert where
  neutral = NullExpert

-- type for all experts
-----------------------

data ExpertType v = forall a . (Expert a, Show a) => ExpertType a v

instance Show (ExpertType v) where
  show (ExpertType e _) = show e

instance Voice v => Expert (ExpertType v) where
  neutral                        = ExpertType NullExpert (head voiceList)
  judge      (ExpertType e _)    = judge e
  judgeGiven (ExpertType e _)    = judgeGiven e
  learnNew1  (ExpertType e v) ev = ExpertType (learnNew1 e ev) v
  learnNew   (ExpertType e v) ev = ExpertType (learnNew e ev) v

expertList :: forall v . (Voice v) => [ExpertType v]
expertList = [ ExpertType (neutral :: WrappedBinExpertUnary (TexturalDensity v)) v
             ]
  where v = head voiceList

-- product of experts
---------------------

data ProductOfExperts v = PoE [ExpertType v]
  deriving (Show)

instance Voice v => Expert (ProductOfExperts v) where
  neutral                  = PoE expertList
  judge      (PoE l) ev    =  product $ map (flip judge ev) l
  judgeGiven (PoE l) e2 e1 =  product $ map (\exp -> judgeGiven exp e2 e1) l
  learnNew1  (PoE l) ev    = PoE (map (flip learnNew1 ev) l)
  learnNew   (PoE l) ep    = PoE (map (flip learnNew ep) l)

-- bin experts
--------------

class BinExpertBase b where
  binNeutral :: b
  binFreq    :: b -> Int -> Float
  binInc     :: b -> Int -> b

class (BinExpertBase b) => BinExpertUnary b where
  binIndex1  :: Voice v => b -> EEvent v -> Int
  binSize1   :: Voice v => EPiece v -> b -> Int -> Int

  binSize1 eEvList exp = (map countSize [0..] !!)
    where indices     = map (binIndex1 exp) eEvList
          countSize i = length $ filter (==i) indices

newtype WrappedBinExpertUnary e = WrapBE1 e
  deriving (Show)

instance (BinExpertUnary b) => Expert (WrappedBinExpertUnary b) where
  neutral = WrapBE1 binNeutral
  judge (WrapBE1 b) ev = (binFreq b i) / (fromIntegral (binSize1 el b i))
    where i  = binIndex1 b ev
          el = tail $ ev : eEventList
  learnNew1 (WrapBE1 b) ev = WrapBE1 $ binInc b (binIndex1 b ev)

class (BinExpertBase b) => BinExpertBinary b where
  binIndex2  :: Voice v => b -> EEvent v -> EEvent v -> Int
  binSize2   :: Voice v => EPiece v -> b -> Int -> Int

  binSize2 eEvList exp = (map countSize [0..] !!)
    where indices     = concatMap (\e -> map (binIndex2 exp e) eEvList) eEvList
          countSize i = length $ filter (==i) indices

newtype WrappedBinExpertBinary e = WrapBE2 e
  deriving (Show)

instance (BinExpertBinary b) => Expert (WrappedBinExpertBinary b) where
  neutral = WrapBE2 binNeutral
  judgeGiven (WrapBE2 b) e2 e1 = (binFreq b i) / (fromIntegral (binSize2 el b i))
    where i  = binIndex2 b e1 e2
          el = tail $ e1 : eEventList
  learnNew (WrapBE2 b) (e1, e2) = WrapBE2 $ binInc b (binIndex2 b e1 e2)

-- textural density rule
------------------------

-- data TexturalDensity' = TexturalDensity' (VU.Vector Float)
--   deriving (Show)

-- restIndex :: Event -> Int
-- restIndex e =
--   foldl (\ acc bit -> 2 * acc + bit) 0 (map (\v -> restBit e v) voiceList)

-- instance Expert TexturalDensity' where
--   neutral = TexturalDensity' (VU.replicate (2 ^ length voiceList) 0)
  
--   judge (TexturalDensity' params) (EEvent event) = params VU.! restIndex event
--   judge _ _ = 1.0 -- EStart and EEnd
  
--   learnNew1 (TexturalDensity' old) (EEvent event) =
--     TexturalDensity' (old VG.// [(i, (old VG.! i) + 1)])
--     where i = restIndex event
--   learnNew1 old _ = old

-- textural density rule (bin implementation
--------------------------------------------

data TexturalDensity v = TexturalDensity v (S.Seq Float)
  deriving (Show)

restBit :: Voice v => Event v -> v -> Int
restBit e v = if isRest $ getEv e v then 1 else 0

tdSizes :: Int -> [Int]
tdSizes vs = (head sizes + 2) : (tail sizes)
  where sizes = map (\i -> 39 ^ (vs - (Bits.popCount i))) ([0..2^vs] :: [Int])

-- lookupBin :: S.Seq Float -> Int -> Float
-- lookupBin s i = case s S.!? i of
--                   (Just f) -> f
--                   Nothing  -> 0

instance Voice v => BinExpertBase (TexturalDensity v) where
  binNeutral = TexturalDensity (head vs) (S.replicate (2 ^ length vs) 0)
    where vs = voiceList
  binFreq (TexturalDensity _ freqs) i = S.index freqs i
  binInc  (TexturalDensity x old) i =
    TexturalDensity x $ S.adjust (+1) i old

instance Voice v => BinExpertUnary (TexturalDensity v) where
  binIndex1 _ (EEvent e) =
      foldl (\ acc bit -> 2 * acc + bit) 0 (map (\v -> restBit e v) voiceList)
  binIndex1 _ _ = 0 -- EStart and EEnd are considered default i.e. no rests

  binSize1 el _ = (tdSizes vl !!) -- precompute sizes
    where (EEvent e) = (head el)
          v = head (voices e)
          vl = length (v:voiceList) - 1

-- TODO:
-- - fix TexturalDensity factors
-- - remaining rule experts
