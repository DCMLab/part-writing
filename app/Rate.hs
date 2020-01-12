module Main where

import VoiceLeading.Base
import VoiceLeading.Distribution
import VoiceLeading.IO.Midi (loadMidi)
import VoiceLeading.IO.Model (loadModel)
import VoiceLeading.IO.LilyPond (viewPieceTmp)

import Options.Applicative as OA
import Data.Semigroup ((<>))
import Control.Monad
import Data.Default

data Opts = Opts
  { oPiece :: FilePath
  , oModel :: FilePath
  , oQuiet :: Bool
  , oShow  :: Bool
  }
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
  <*> switch
  ( long "show"
    <> short 's'
    <> help "print the loaded piece; only works with --quiet/-q" )

optsPInfo = info (optsParser <**> helper)
  (fullDesc <> progDesc "Rate the VL quality of a piece")

filterGaps :: Voice v => Piece v -> Piece v
filterGaps (Piece meta events) = Piece meta evs'
  where evs' = filter (not . isEmptyEvent . removeRests) events

main :: IO ()
main = do
  opts <- execParser optsPInfo
  piece' <- (loadMidi $ oPiece opts) :: IO (Piece ChoralVoice)
  let piece = filterGaps piece'
  model <- (loadModel $ oModel opts) :: IO (Model ChoralVoice)
  let aopts = def -- TODO: load harmony profiles
  if oQuiet opts
    then putStrLn (show $ evalPieceUnnormLog aopts piece model)
    else do
    when (oShow opts) $ do
      forM_ (pieceEvents piece) (putStrLn . show)
    putStrLn $ "piece rating:       " <> (show $ evalPieceUnnorm aopts piece model)
    putStrLn $ "log piece rating:   " <> (show $ evalPieceUnnormLog aopts piece model)
    putStrLn $ "mean log potential: " <> (show $ meanLogPotential aopts piece model)
