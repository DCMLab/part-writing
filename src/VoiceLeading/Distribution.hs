{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module VoiceLeading.Distribution where

import VoiceLeading.Base
import VoiceLeading.Automaton

import Data.List (transpose, findIndex, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)

import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Text (Text, unpack)
import qualified Data.ByteString.Lazy as B

-----------------
-- basic types --
-----------------

type ModelParams = [Double]

type FeatureCounts = [Double]

data Model v = Model { modelFeatures :: [NamedFeature v]
                     , modelParams :: ModelParams }

instance Show v => Show (Model v) where
  show (Model fs ps) = "Model:\n" ++ concat (zipWith showF fs ps)
    where showF (NamedFeature _ n v) p = (unpack n) ++ " " ++ show p ++ "\n"

----------------
-- evaluation --
----------------

-- countTrue :: [Bool] -> Double
-- countTrue = fromIntegral . length . filter id

countFeatures :: Voice v => Piece v -> [Feature v] -> FeatureCounts
countFeatures piece feats = map sum trans
  where trans = transpose $ runFeaturesOn piece feats

evalModelUnnormLog :: FeatureCounts -> ModelParams -> Double
evalModelUnnormLog fs thetas
  -- | if any (>0) fs then DT.trace ("positive features: " ++ show fs) False else False = undefined
  | length fs == length thetas = sum (zipWith (*) fs thetas)
  | otherwise = error $ "feature counts and params vectors must have the same length:\
                        \|fs| = " ++ show (length fs) ++ ", |thetas| = " ++ show (length thetas)
        
evalModelUnnorm :: FeatureCounts -> ModelParams -> Double
evalModelUnnorm fs thetas = exp $ evalModelUnnormLog fs thetas - m
  where m = maximum thetas

evalModel :: FeatureCounts -> ModelParams -> Double -> Double
evalModel fs thetas z = evalModelUnnorm fs thetas / z

evalPieceUnnorm :: Voice v => Piece v -> Model v -> Double
evalPieceUnnorm piece (Model nfeats params) =
  evalModelUnnorm (countFeatures piece (unname nfeats)) params

evalPieceUnnormLog :: Voice v => Piece v -> Model v -> Double
evalPieceUnnormLog piece (Model nfeats params) =
  evalModelUnnormLog (countFeatures piece (unname nfeats)) params

meanLogPotential :: Voice v => Piece v -> Model v -> Double
meanLogPotential piece model =
  evalPieceUnnormLog piece model / fromIntegral (pieceLen piece)

meanLogPotentialN :: Voice v => [Piece v] -> Model v -> Double
meanLogPotentialN ps model =
  let nevs   = sum $ map pieceLen ps
      logpot = sum $ map (flip evalPieceUnnormLog model) ps in
    logpot / fromIntegral nevs

meanFeatCounts :: Voice v => Piece v -> Model v -> [Double]
meanFeatCounts piece (Model nfeats _) =
  map (/ fromIntegral (pieceLen piece)) (countFeatures piece (unname nfeats))

expectedFeats :: Voice v => [Piece v] -> [Feature v] -> [Double]
expectedFeats pieces features = map (/n) counts
  where n      = fromIntegral $ sum (map pieceLen pieces)
        counts = foldr1 (zipWith (+)) (map ((flip countFeatures) features) pieces)

meanFeatCountsN :: Voice v => [Piece v] -> Model v -> [Double]
meanFeatCountsN ps (Model nfeats _) = expectedFeats ps (unname nfeats)

-- helper
unname :: [NamedFeature v] -> [Feature v]
unname nfs = map nfFeature nfs

------------------------
-- saving and loading --
------------------------

instance ToJSON (Model v) where
  toJSON (Model feats params) = object $ zipWith mkPair feats params
    where mkPair f p = nfName f .= p

instance FromJSON (Model ChoralVoice) where
  parseJSON object = do
    mm <- parseJSON object :: Parser (M.Map Text Double)
    let pairs = map fst $ sortOn snd (mapMaybe mkTriple (M.toList mm))
    pure $ Model (map fst pairs) (map snd pairs)
    where mkTriple (str,val) = case findIndex (\nf -> nfName nf == str) defaultFeaturesNamed of
                               Nothing -> Nothing
                               Just i -> Just ((defaultFeaturesNamed !! i, val), i)

data ModelFile v = ModelFile
                 { mfDesc  :: Maybe String
                 , mfOpts  :: Maybe String
                 , mfModel :: (Model v)}
  deriving (Show)

instance FromJSON (ModelFile ChoralVoice) where
  parseJSON = withObject "ModelFile" $ \o -> ModelFile
    <$> o .: "description"
    <*> o .: "options"
    <*> o .: "model"

instance ToJSON (ModelFile v) where
  toJSON (ModelFile desc opts model) =
    object [ "description" .= desc
           , "options" .= opts
           , "model" .= model ]

saveModel :: Model v -> String -> String -> FilePath -> IO ()
saveModel model desc opts fp = B.writeFile fp (encodePretty mf)
  where mf = ModelFile (Just desc) (Just opts) model

loadModel :: FilePath -> IO (Model ChoralVoice)
loadModel fp = do
  str <- B.readFile fp
  let mf = (decode str)-- :: Maybe (ModelFile ChoralVoice)
  case mf of
    Just m  -> pure $ mfModel m
    Nothing -> error "could not parse model"
