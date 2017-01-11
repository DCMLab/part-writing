{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ExistentialQuantification #-}

{- |
Module: VoiceLeading.Prob
Description : Probabilistic voice leading rules
Copyright   : (c) Christoph Finkensiep, 2017
License     : MIT
Maintainer  : chfin@freenet.de
Stability   : experimental
Portability : POSIX

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
-}
module VoiceLeading.Prob
  ( Expert(..)
  , ProductOfExperts(..)
  , TexturalDensity(..)
  , learn, learn1, learnAll, learnAll1
  ) where

import VoiceLeading.Base
import VoiceLeading.Load
import qualified Data.Vector as VB
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG
import Data.Dynamic

import qualified Debug.Trace as DT

--------- --
-- Expert --
------------

class Expert e where
  neutral    :: e
  judge      :: e -> EEvent -> Float
  judgeGiven :: e -> EEvent -> EEvent -> Float
  learnNew1  :: e -> EEvent -> e
  learnNew   :: e -> (EEvent, EEvent) -> e

  judge      _ _        = 1.0
  judgeGiven exp e2 _   = judge exp e2
  learnNew1  exp _      = exp
  learnNew   exp (_,e2) = learnNew1 exp e2

learn :: (Expert e) => e -> EPiece -> e
learn expert piece = VG.foldl learnNew expert (VG.zip (VG.init piece) (VG.tail piece))

learn1 :: (Expert e) => EPiece -> e
learn1 = learn neutral

learnAll :: (Expert e) => e -> Pieces -> e
learnAll expert pieces = VG.foldl learn expert (VG.map toEPiece pieces)

learnAll1 :: (Expert e) => Pieces -> e
learnAll1 = learnAll neutral

judgeNorm :: (Expert e) => e -> EEvent -> Float
judgeNorm expert event = (judge expert event) / z
  where z = VG.sum $ VG.map (judge expert) eEventList

judgeGivenNorm :: (Expert e) => e -> EEvent -> EEvent -> Float
judgeGivenNorm expert e2 e1 = (judgeGiven expert e2 e1) / z
  where z = VG.sum $ VG.map (\e -> judgeGiven expert e e1) eEventList

-- null expert type
-------------------

data NullExpert = NullExpert
  deriving (Show)

instance Expert NullExpert where
  neutral = NullExpert

-- type for all experts
-----------------------

data ExpertType = forall a . (Expert a, Show a) => ExpertType a

instance Show ExpertType where
  show (ExpertType e) = show e

instance Expert ExpertType where
  neutral                   = ExpertType NullExpert
  judge      (ExpertType e) = judge e
  judgeGiven (ExpertType e) = judgeGiven e
  learnNew1  (ExpertType e) = ExpertType . learnNew1 e
  learnNew   (ExpertType e) = ExpertType . learnNew e

expertList :: [ExpertType]
expertList = [ ExpertType (neutral :: TexturalDensity)
             ] -- should contain neutral of every expert type

-- product of experts
---------------------

data ProductOfExperts = PoE [ExpertType]
  deriving (Show)

instance Expert ProductOfExperts where
  neutral                  = PoE expertList
  judge      (PoE l) ev    =  product $ map (flip judge ev) l
  judgeGiven (PoE l) e2 e1 =  product $ map (\exp -> judgeGiven exp e2 e1) l
  learnNew1  (PoE l) ev    = PoE (map (flip learnNew1 ev) l)
  learnNew   (PoE l) ep    = PoE (map (flip learnNew ep) l)

-- textural density rule
------------------------

data TexturalDensity = TexturalDensity (VU.Vector Float)
  deriving (Show)

restBit :: Event -> Voice -> Int
restBit e v = if isRest $ getEv e v then 1 else 0

restIndex :: Event -> Int
restIndex e =
  foldl (\ acc bit -> 2 * acc + bit) 0 (map (\v -> restBit e v) voiceList)

instance Expert TexturalDensity where
  neutral = TexturalDensity (VU.replicate (2 ^ length voiceList) 0)
  
  judge (TexturalDensity params) (EEvent event) = params VU.! restIndex event
  judge _ _ = 1.0 -- EStart and EEnd
  
  learnNew1 (TexturalDensity old) (EEvent event) =
    TexturalDensity (old VG.// [(i, (old VG.! i) + 1)])
    where i = restIndex event
  -- learnNew1 t e@(EEvent _) = learnNew1 t e
  learnNew1 old _ = old
  
-- TODO:
-- - fix TexturalDensity factors
-- - remaining rule experts
