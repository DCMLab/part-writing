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

strToVoice str = case str of
    "s" -> Just Soprano
    "a" -> Just Alto
    "t" -> Just Tenor
    "b" -> Just Bass
    _   -> Nothing

instance FromJSON Voice where
  parseJSON str = case strToVoice str of
    Just v  -> return v
    Nothing -> typeMismatch "Voice" str

instance FromJSONKey Voice where
  fromJSONKey = FromJSONKeyTextParser $ \t -> case (strToVoice t) of
    Just v  -> pure v
    Nothing -> typeMismatch "Voice" (String t)

instance FromJSON Pitch where
  parseJSON (Number (-1)) = return Rest
  parseJSON (Number i) = return $ Pitch (round i)
  parseJSON invalid = typeMismatch "Pitch" invalid

instance FromJSON Event where
  parseJSON object = do
    mm <- parseJSON object :: Parser (M.Map Voice Pitch)
    return (toEv mm)
--    typeMismatch "Event" object
--  parseJSON invalid = typeMismatch "Event" invalid

decodePieces :: B.ByteString -> Maybe Pieces
decodePieces b = decode b

loadPieces :: FilePath -> IO Pieces
loadPieces fn = do
  str <- B.readFile fn
  case decodePieces str of
    Just p  -> return p
    Nothing -> fail "could not parse json"
