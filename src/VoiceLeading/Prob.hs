{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ExistentialQuantification #-}

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
-- import qualified Data.Vector.Generic.Mutable as VGM
import Data.Dynamic

import qualified Debug.Trace as DT

--------- --
-- Expert --
------------

data EEvent = EStart | EEvent Event | EEnd
  deriving (Show)

toEEv = EEvent

type EPiece = VB.Vector EEvent

toEPiece piece = (VB.singleton EStart) VG.++
                 (VG.map toEEv piece) VG.++
                 (VB.singleton EEnd)

class Expert e where
  neutral    :: e
--  combine    :: e -> e -> e
  normalize  :: e -> e
  denorm     :: e -> e
  judge      :: e -> EEvent -> Float
  judgeGiven :: e -> EEvent -> EEvent -> Float
  learnNew1  :: e -> EEvent -> e
  learnNew   :: e -> (EEvent, EEvent) -> e

  normalize  exp        = exp
  denorm     exp        = exp
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
  normalize  (ExpertType e) = ExpertType $ normalize e
  denorm     (ExpertType e) = ExpertType $ denorm e
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
  normalize  (PoE l)       = PoE $ map normalize l
  denorm     (PoE l)       = PoE $ map denorm l
  judge      (PoE l) ev    = product $ map (flip judge ev) l
  judgeGiven (PoE l) e2 e1 = product $ map (\exp -> judgeGiven exp e1 e2) l
  learnNew1  (PoE l) ev    = PoE $ map (flip learnNew1 ev) l
  learnNew   (PoE l) ep    = PoE $ map (flip learnNew ep) l

-- textural density rule
------------------------

data TexturalDensity = TexturalDensity (VU.Vector Float) Float
  deriving (Show)

restBit :: Event -> Voice -> Int
restBit e v = if isRest $ getEv e v then 1 else 0

restIndex :: Event -> Int
restIndex e =
  foldl (\ acc bit -> 2 * acc + bit) 0 (map (\v -> restBit e v) voiceList)

instance Expert TexturalDensity where
  neutral = TexturalDensity (VU.replicate (2 ^ length voiceList) 0) 1

  -- combine (TexturalDensity p1 1.0) (TexturalDensity p2 1.0) =
  --   TexturalDensity (VG.zipWith (+) p1 p2) 1.0
  -- combine t1 t2 = combine (denorm t1) (denorm t2)
  
  normalize (TexturalDensity params f) =
    TexturalDensity (VG.map (/sum) params) (f*sum)
    where sum = VG.sum params

  denorm t@(TexturalDensity p 1.0) = t
  denorm (TexturalDensity p f) = TexturalDensity (VG.map (*f) p) 1.0

  judge (TexturalDensity params f) (EEvent event) = params VU.! restIndex event
  
  learnNew1 (TexturalDensity old 1.0) (EEvent event) =
    TexturalDensity (old VG.// [(i, (old VG.! i) + 1)]) 1.0
    where i = restIndex event
  learnNew1 t e@(EEvent _) = learnNew1 (denorm t) e
  learnNew1 old _ = old

-- TODO:
-- - ProductOfExperts, which implements Expert
-- - DONE: proper normalization
--   - should result in real probabilities
--   - only allowed in the end, cannot be further combined or learned
--     -> add denorm, generalize combine and learnNew(1)
--   - monad? -> no
-- - remaining rule experts
