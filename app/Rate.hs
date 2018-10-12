module Main where

import VoiceLeading.Base
import VoiceLeading.Distribution
import VoiceLeading.IO.Midi (loadMidi)
import VoiceLeading.IO.Model (loadModel)

import Options.Applicative as OA
import Data.Semigroup ((<>))

data Opts = Opts
  { oPiece :: FilePath
  , oModel :: FilePath
  , oQuiet :: Bool }
  deriving (Show)

optsParser :: OA.Parser Opts
optsParser = Opts
  <$> strArgument
  ( help "piece to be loaded (midi file)"
    <> metavar "piece" )
  <*> strOption
  ( long "model"
    <> short 'm'
    <> help "model file, contains the parameters"
    <> showDefault
    <> value "model.json"
    <> metavar "model" )
  <*> switch
  ( long "quiet"
    <> short 'q'
    <> help "output just a single number as result" )

optsPInfo = info (optsParser <**> helper)
  (fullDesc <> progDesc "Rate the VL quality of a piece")

main :: IO ()
main = do
  opts <- execParser optsPInfo
  piece <- (loadMidi $ oPiece opts) :: IO (Piece ChoralVoice)
  model <- (loadModel $ oModel opts) :: IO (Model ChoralVoice)
  if oQuiet opts
    then putStrLn (show $ evalPieceUnnormLog piece model)
    else do
    putStrLn $ "piece rating:       " <> (show $ evalPieceUnnorm piece model)
    putStrLn $ "log piece rating:   " <> (show $ evalPieceUnnormLog piece model)
    putStrLn $ "mean log potential: " <> (show $ meanLogPotential piece model)
