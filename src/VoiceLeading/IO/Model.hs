{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : VoiceLeading.Distribution
Description : Import and export for models
Copyright   : (c) Christoph Finkensiep, 2017
License     : MIT
Maintainer  : chfin@chfin.de
Stability   : experimental
Portability : POSIX

This module provides simple import and export for 'Model's.
-}
module VoiceLeading.IO.Model
  ( loadModel
  , saveModel
  )
where

import           VoiceLeading.Base
import           VoiceLeading.Distribution      ( Model(..) )
import           VoiceLeading.Automaton         ( nfName
                                                , defaultFeaturesNamed
                                                )

import           Data.List                      ( findIndex
                                                , sortOn
                                                )
import qualified Data.Map.Strict               as M
import           Data.Maybe                     ( mapMaybe )
import qualified Data.Vector                   as V
import qualified Data.Vector.Unboxed           as VU

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Aeson.Encode.Pretty       ( encodePretty )
import           Data.Text                      ( Text
                                                , unpack
                                                )
import qualified Data.ByteString.Lazy          as B

instance ToJSON (Model v) where
  toJSON (Model feats params) = object $ V.toList $ V.zipWith
    mkPair
    feats
    (V.convert params)
    where mkPair f p = nfName f .= p

instance FromJSON (Model ChoralVoice) where
  parseJSON object = do
    mm <- parseJSON object :: Parser (M.Map Text Double)
    let pairs =
          V.fromList $ fst <$> sortOn snd (mapMaybe mkTriple (M.toList mm))
    pure $ Model (fst <$> pairs) (VU.convert $ snd <$> pairs)
   where
    mkTriple (str, val) =
      case findIndex (\nf -> nfName nf == str) defaultFeaturesNamed of
        Nothing -> Nothing
        Just i  -> Just ((defaultFeaturesNamed !! i, val), i)

data ModelFile v = ModelFile
                 { mfDesc  :: Maybe String
                 , mfOpts  :: Maybe String
                 , mfModel :: (Model v)}
  deriving (Show)

instance FromJSON (ModelFile ChoralVoice) where
  parseJSON = withObject "ModelFile" $ \o ->
    ModelFile <$> o .: "description" <*> o .: "options" <*> o .: "model"

instance ToJSON (ModelFile v) where
  toJSON (ModelFile desc opts model) =
    object ["description" .= desc, "options" .= opts, "model" .= model]

-- | Saves a model to a JSON file.
--   Takes two strings for a description of the file and
--   a string representation of the used options, respectively.
--   These are only used for a external (e.g. human) readers of the file.
saveModel :: Model v -> String -> String -> FilePath -> IO ()
saveModel model desc opts fp = B.writeFile fp (encodePretty mf)
  where mf = ModelFile (Just desc) (Just opts) model

-- | Loads a model from a file.
--   The description and option fields are discarded, just the model is loaded.
loadModel :: FilePath -> IO (Model ChoralVoice)
loadModel fp = do
  str <- B.readFile fp
  let mf = (decode str)-- :: Maybe (ModelFile ChoralVoice)
  case mf of
    Just m  -> pure $ mfModel m
    Nothing -> error "could not parse model"
