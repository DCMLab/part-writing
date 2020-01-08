{-# LANGUAGE OverloadedStrings #-}

module Main where

import VoiceLeading.Base
import VoiceLeading.Automaton (nfName)
import VoiceLeading.Inference ( estimateGibbsNotes, uniformRandomPiece'
                              , mapEstimateNotewise)
import VoiceLeading.Distribution ( modelFeatures
                                 , meanLogPotential, meanLogPotentialN
                                 , meanFeatCounts, meanFeatCountsN)
import VoiceLeading.Helpers (RFun(..), rFun)

import VoiceLeading.IO.Midi (corpusDir, corpusPieces, testPiece, loadMidi)
import VoiceLeading.IO.LilyPond (viewPiece, viewPieceTmp)
import VoiceLeading.IO.Plotting
import VoiceLeading.IO.Model (loadModel)

import Options.Applicative as OA
import Data.Semigroup ((<>))
import Data.Yaml as Yaml
import qualified Data.Text as T
import Control.Monad (unless, when)

data PieceStart = TestPiece
                | NewPiece Int
                | CorpusPiece FilePath
                | File FilePath
  deriving (Eq, Show, Read)

data Opts = Opts
  { iterations :: Int
  , startWith  :: PieceStart
  , keepVoices :: [ChoralVoice]
  , fPower     :: RFun
  , modelFp    :: FilePath
  , outputFp   :: FilePath
  , diagramFp  :: FilePath
  , featuresFp :: FilePath
  , compareCrp :: Bool
  , comparePfx :: FilePath
  , quiet      :: Bool }
  deriving (Show)

defaultOpts = Opts
  { iterations = 20
  , startWith = TestPiece
  , keepVoices = []
  , fPower = Cnst 5.0
  , modelFp = "model.json"
  , outputFp = ""
  , diagramFp = "diagram.pdf"
  , featuresFp = "estimate_feats.pdf"
  , compareCrp = False
  , comparePfx = ""
  , quiet = False }

parseRead :: Read a => String -> Yaml.Value -> Yaml.Parser a
parseRead expected = withText expected $ \v -> pure (read (T.unpack v))

instance FromJSON PieceStart where
  parseJSON = parseRead "TestPiece | NewPiece INT | CorpusPiece NAME | File FILE"

instance FromJSON ChoralVoice where
  parseJSON = parseRead "ChoralVoice"

instance FromJSON RFun where
  parseJSON = parseRead "RFun"

instance FromJSON Opts where
  parseJSON = withObject "Opts" $ \v -> Opts
    <$> v .:? "iterations" .!= (iterations defaultOpts)
    <*> v .:? "start-with" .!= (startWith defaultOpts)
    <*> v .:? "keep-voices" .!= (keepVoices defaultOpts)
    <*> v .:? "power" .!= (fPower defaultOpts)
    <*> v .:? "model" .!= (modelFp defaultOpts)
    <*> v .:? "output" .!= (outputFp defaultOpts)
    <*> v .:? "diagram-out" .!= (diagramFp defaultOpts)
    <*> v .:? "feature-summary-out" .!= (featuresFp defaultOpts)
    <*> pure False
    <*> v .:? "compare-corpus-prefix" .!= (comparePfx defaultOpts)
    <*> pure False

data Conf = Conf { confCompose :: Opts }

instance FromJSON Conf where
  parseJSON = withObject "Conf" $ \v -> Conf
    <$> v .:? "compose" .!= defaultOpts

loadConfig :: FilePath -> IO Opts
loadConfig fp = do
  conf <- decodeFileEither fp
  pure $ either (const defaultOpts) confCompose conf

opts :: Opts -> OA.Parser Opts
opts defs = Opts
  <$> argument auto
  ( help "number of estimation iterations"
    <> showDefault
    <> value (iterations defs)
    <> metavar "iterations" )
  <*> option auto
  ( long "start-with"
    <> short 's'
    <> help "initializes the estimation procedure"
    <> showDefault
    <> value (startWith defs)
    <> metavar "TestPiece | \"NewPiece INT\" | \"CorpusPiece NAME\" | \"File FILE\"")
  <*> option auto
  ( long "keep-voices"
    <> short 'k'
    <> help "keep these voices from the initial piece (comma separated in \"[]\")"
    <> showDefault
    <> value (keepVoices defs)
    <> metavar "\"[(Soprano|Alto|Tenor|Bass)*]\"")
  <*> option auto
  ( long "power"
    <> short 'p'
    <> help "the value of the annealing power (over time)"
    <> showDefault
    <> value (fPower defs)
    <> metavar "RFUN")
  <*> strOption
  ( long "model"
    <> short 'm'
    <> help "model file used for estimation"
    <> showDefault
    <> value (modelFp defs)
    <> metavar "FILE" )
  <*> strOption
  ( long "output"
    <> short 'o'
    <> help "LilyPond filename of final composition. If empty, a temporary file will be used."
    <> showDefault
    <> value (outputFp defs)
    <> metavar "FILE" )
  <*> strOption
  ( long "diagram-out"
    <> short 'd'
    <> help "filename for the model diagram"
    <> showDefault
    <> value (diagramFp defs)
    <> metavar "FILE" )
  <*> strOption
  ( long "feature-summary-out"
    <> short 'f'
    <> help "filename for the feature summary diagram"
    <> showDefault
    <> value (featuresFp defs)
    <> metavar "FILE" )
  <*> switch
  ( long "compare-corpus"
    <> short 'c'
    <> help "plot a comparison of the piece with corpus data" )
  <*> strOption
  ( long "compare-corpus-prefix"
    <> showDefault
    <> value (comparePfx defs)
    <> help "corpus comparison filenames use the prefix PREFIX"
    <> metavar "PREFIX")
    <*> switch
  ( long "no-plots" <> short 'q' <> help "don't write plot files")

optsInfo defs = info (opts defs <**> helper)
  (fullDesc <> progDesc "Compose a piece by MAP estimation")

main :: IO ()
main = do
  defs <- loadConfig "defaults.yaml"
  -- read options
  options <- execParser $ optsInfo defs
  putStrLn $ show options
  let q = quiet options

  -- load model and plot its parameters
  model <- loadModel $ modelFp options
  let mfNames = map nfName (modelFeatures model)
  unless (q || null (diagramFp options)) $
    plottingLogger (diagramFp options) model

  -- load / generate initial piece
  piece <- case (startWith options) of
            TestPiece      -> testPiece
            NewPiece len   -> uniformRandomPiece' len
            CorpusPiece fp -> loadMidi $ corpusDir ++ fp
            File fp        -> loadMidi fp

  -- MAP estimation
  est' <- estimateGibbsNotes (keepVoices options) piece model
          (iterations options) (rFun $ fPower options)
  est <- mapEstimateNotewise (Just est') model 0 (keepVoices options)

  -- plot summary of estimated piece 
  putStrLn $ "logpot/event estimate: " ++ show (meanLogPotential est model)
  let fsfp    = featuresFp options
      avgFEst = meanFeatCounts est model
  unless (q || null fsfp) $
    plotOverFeatures fsfp
      "Mean Feature Values (MAP Estimate)"
      mfNames avgFEst

  -- plot comparision with corpus data
  when (compareCrp options) $ do
    ps <- corpusPieces
    putStrLn $ "logpot/event corpus: " ++ show (meanLogPotentialN ps model)
    unless q $ do
      let pfx = comparePfx options
      let avgFCorp = meanFeatCountsN ps model
      plotOverFeatures (pfx ++ "corpus_feats.pdf")
        "Mean Feature Values (Corpus)"
        mfNames avgFCorp
      let avgFDiff = map log $ zipWith (/) avgFEst avgFCorp
      plotOverFeatures (pfx ++ "relative_feats.pdf")
        "Mean Feature Values (Estimate/Corpus)"
        mfNames avgFDiff

  -- view the generated piece
  if (null $ outputFp options)
    then viewPieceTmp est
    else viewPiece est (outputFp options)
