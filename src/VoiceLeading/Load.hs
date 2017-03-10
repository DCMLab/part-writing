{-# LANGUAGE OverloadedStrings #-}

module VoiceLeading.Load
  ( decodePieces
  , loadPieces)
  where

import VoiceLeading.Base
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Lazy as B

-- import qualified Debug.Trace as DT

---------------------------------------------------------
-- TODO: Adapt json format to actual piece representation
---------------------------------------------------------

strToVoice str = case str of
    "s" -> Just Soprano
    "a" -> Just Alto
    "t" -> Just Tenor
    "b" -> Just Bass
    _   -> Nothing

instance FromJSON ChoralVoice where
  parseJSON str = case strToVoice str of
    Just v  -> return v
    Nothing -> typeMismatch "Voice" str

instance FromJSONKey ChoralVoice where
  fromJSONKey = FromJSONKeyTextParser $ \t -> case (strToVoice t) of
    Just v  -> pure v
    Nothing -> typeMismatch "Voice" (String t)

-- TODO: parse holds correctly
instance FromJSON Pitch where
  parseJSON (Number (-1)) = return Rest
  parseJSON (Number i) = return $ Pitch (round i) False
  parseJSON invalid = typeMismatch "Pitch" invalid

-- TODO: parse with beat information
instance (FromJSONKey v, Voice v) => FromJSON (Event v) where
  parseJSON object = do
    mm <- parseJSON object :: (FromJSONKey v, Voice v) => Parser (M.Map v Pitch)
    return (toEv mm 0)

instance (FromJSONKey v, Voice v) => FromJSON (Piece v) where
  parseJSON object = do
    events <- parseJSON object :: (FromJSONKey v, Voice v) => Parser [Event v]
    return $ Piece nullPieceMeta events

decodePieces :: (FromJSONKey v, Voice v) => B.ByteString -> Maybe (Pieces v)
decodePieces b = decode b

loadPieces :: (FromJSONKey v, Voice v) => FilePath -> IO (Pieces v)
loadPieces fn = do
  str <- B.readFile fn
  case decodePieces str of
    Just p  -> return p
    Nothing -> fail "could not parse json"
