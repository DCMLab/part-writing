{-# LANGUAGE OverloadedStrings #-}

module Main where

import           VoiceLeading.Base
import           VoiceLeading.Automaton         ( AutoOpts(..) )
import           VoiceLeading.Features          ( nfName )
import           VoiceLeading.Inference         ( estimateGibbsNotes
                                                , uniformRandomPiece
                                                , randomizePiece
                                                , InfLogger
                                                )
import           VoiceLeading.Distribution      ( modelFeatures
                                                , meanLogPotential
                                                , meanLogPotentialN
                                                , meanFeatCounts
                                                , meanFeatCountsN
                                                , evalPieceUnnormLog
                                                )
import           VoiceLeading.Helpers           ( parseRead
                                                , defaultSeed
                                                )
import           VoiceLeading.Theory            ( loadProfiles
                                                , vectorizeProfiles
                                                , matchChordProfiles
                                                )

import           VoiceLeading.IO.Midi           ( corpusDir
                                                , corpusPieces
                                                , testPiece
                                                , loadMidi
                                                )
import           VoiceLeading.IO.LilyPond       ( viewPiece
                                                , viewPieceTmp
                                                )
import           VoiceLeading.IO.Plotting
import           VoiceLeading.IO.Model          ( loadModel )

import           Options.Applicative           as OA
import           Data.Semigroup                 ( (<>) )
import           Data.Yaml                     as Yaml
import           Control.Monad                  ( unless
                                                , when
                                                , replicateM
                                                )
import           Data.Default
import qualified Data.Vector.Unboxed           as VU
import qualified Data.Function.Memoize         as Mem
import           System.Random.MWC              ( initialize )

data PieceStart = TestPiece
                | NewPiece Int
                | CorpusPiece FilePath
                | File FilePath
  deriving (Eq, Show, Read)

data Opts = Opts
  { iterations :: Int
  , cooling    :: Double
  , tempEnd    :: Double
  , startWith  :: PieceStart
  , keepVoices :: [ChoralVoice]
  , profileFp  :: FilePath
  , modelFp    :: FilePath
  , outputFp   :: FilePath
  , featuresFp :: FilePath
  , compareCrp :: Bool
  , comparePfx :: FilePath
  , quiet      :: Bool }
  deriving (Show)

defaultOpts :: Opts
defaultOpts = Opts { iterations = 20
                   , cooling    = 0.7
                   , tempEnd    = 0.1
                   , startWith  = TestPiece
                   , keepVoices = []
                   , profileFp  = "data/jsbach_chorals_harmony/profiles.json"
                   , modelFp    = "model.json"
                   , outputFp   = ""
                   , featuresFp = "estimate_feats.pdf"
                   , compareCrp = False
                   , comparePfx = ""
                   , quiet      = False
                   }

instance FromJSON PieceStart where
  parseJSON =
    parseRead "TestPiece | NewPiece INT | CorpusPiece NAME | File FILE"

instance FromJSON Opts where
  parseJSON = withObject "Opts" $ \v ->
    Opts
      <$> v
      .:? "iterations"
      .!= (iterations defaultOpts)
      <*> v
      .:? "cooling"
      .!= (cooling defaultOpts)
      <*> v
      .:? "temp"
      .!= (tempEnd defaultOpts)
      <*> v
      .:? "start-with"
      .!= (startWith defaultOpts)
      <*> v
      .:? "keep-voices"
      .!= (keepVoices defaultOpts)
      <*> v
      .:? "profiles"
      .!= (profileFp defaultOpts)
      <*> v
      .:? "model"
      .!= (modelFp defaultOpts)
      <*> v
      .:? "output"
      .!= (outputFp defaultOpts)
      <*> v
      .:? "feature-summary-out"
      .!= (featuresFp defaultOpts)
      <*> pure False
      <*> v
      .:? "compare-corpus-prefix"
      .!= (comparePfx defaultOpts)
      <*> pure False

data Conf = Conf { confCompose :: Opts }

instance FromJSON Conf where
  parseJSON =
    withObject "Conf" $ \v -> Conf <$> v .:? "compose" .!= defaultOpts

loadConfig :: FilePath -> IO Opts
loadConfig fp = do
  conf <- decodeFileEither fp
  pure $ either (const defaultOpts) confCompose conf

opts :: Opts -> OA.Parser Opts
opts defs =
  Opts
    <$> argument
          auto
          (  help "minimum number of estimation iterations"
          <> showDefault
          <> value (iterations defs)
          <> metavar "iterations"
          )
    <*> option
          auto
          (  long "cooling"
          <> short 'k'
          <> help "cooling factor (between 0 and 1)"
          <> showDefault
          <> value (cooling defs)
          <> metavar "(0,1]"
          )
    <*> option
          auto
          (  long "temp-end"
          <> short 't'
          <> help "target temperature"
          <> showDefault
          <> value (tempEnd defs)
          <> metavar "Double"
          )
    <*> option
          auto
          (  long "start-with"
          <> short 's'
          <> help "initializes the estimation procedure"
          <> showDefault
          <> value (startWith defs)
          <> metavar
               "TestPiece | \"NewPiece INT\" | \"CorpusPiece NAME\" | \"File FILE\""
          )
    <*> option
          auto
          (  long "keep-voices"
          <> short 'v'
          <> help
               "keep these voices from the initial piece (comma separated in \"[]\")"
          <> showDefault
          <> value (keepVoices defs)
          <> metavar "\"[(Soprano|Alto|Tenor|Bass)*]\""
          )
    <*> strOption
          (  long "chord-profile"
          <> short 'P'
          <> help "the file from which to read the chord profiles"
          <> showDefault
          <> value (profileFp defs)
          <> metavar "FILE"
          )
    <*> strOption
          (  long "model"
          <> short 'm'
          <> help "model file used for estimation"
          <> showDefault
          <> value (modelFp defs)
          <> metavar "FILE"
          )
    <*> strOption
          (  long "output"
          <> short 'o'
          <> help
               "LilyPond filename of final composition. If empty, a temporary file will be used."
          <> showDefault
          <> value (outputFp defs)
          <> metavar "FILE"
          )
    <*> strOption
          (  long "feature-summary-out"
          <> short 'f'
          <> help "filename for the feature summary diagram"
          <> showDefault
          <> value (featuresFp defs)
          <> metavar "FILE"
          )
    <*> switch
          (long "compare-corpus" <> short 'c' <> help
            "plot a comparison of the piece with corpus data"
          )
    <*> strOption
          (  long "compare-corpus-prefix"
          <> showDefault
          <> value (comparePfx defs)
          <> help "corpus comparison filenames use the prefix PREFIX"
          <> metavar "PREFIX"
          )
    <*> switch (long "no-plots" <> short 'q' <> help "don't write plot files")

optsInfo :: Opts -> ParserInfo Opts
optsInfo defs = info
  (opts defs <**> helper)
  (fullDesc <> progDesc "Compose a piece by MAP estimation")

-- logging
----------

consoleLogger :: InfLogger IO
consoleLogger it pot temp dC dS =
  putStrLn
    $  "k = "
    <> show it
    <> "\tpot = "
    <> show pot
    <> "\ttemp = "
    <> show temp
    <> "\tdC = "
    <> show dC
    <> "\tdS = "
    <> show dS

-- main
-------

main :: IO ()
main = do
  -- random state
  gen     <- initialize defaultSeed -- for reproducability

  -- read options
  defs    <- loadConfig "defaults.yaml"
  options <- execParser $ optsInfo defs
  print options
  let q = quiet options

  -- load model and plot its parameters
  model <- loadModel $ modelFp options
  let mfNames = nfName <$> modelFeatures model

  -- load / generate initial piece
  loadedPiece <- case startWith options of
    TestPiece       -> testPiece
    NewPiece    len -> uniformRandomPiece len gen
    CorpusPiece fp  -> loadMidi $ corpusDir ++ fp
    File        fp  -> loadMidi fp
  piece      <- randomizePiece gen (keepVoices options) loadedPiece

  -- load chord profiles
  (Just pfs) <- loadProfiles $ profileFp options
  let profiles = vectorizeProfiles pfs
      harmEst  = Mem.memoize $ matchChordProfiles profiles
      aopts    = def { oHarmEstimator = harmEst }

  -- initial temperature (t0)
  let nSamples = 20 -- number of comparison samples for initial temperature
  randomSteps <- replicateM nSamples
    $ randomizePiece gen (keepVoices options) piece
  let meanC =
        sum ((\p -> abs $ evalPieceUnnormLog aopts p model) <$> randomSteps)
          / fromIntegral nSamples
      t0 :: Double
      t0   = negate $ meanC / log 0.999
      tEnd = tempEnd options
      kA   = cooling options


  -- logging
  let logger = consoleLogger

  -- MAP estimation
  est' <- estimateGibbsNotes gen
                             aopts
                             (keepVoices options)
                             piece
                             model
                             t0
                             tEnd
                             kA
                             (iterations options)
                             10
                             logger
  -- TODO: fix broken maximization
  -- est <- mapEstimateNotewise aopts (Just est') model 0 (keepVoices options)
  let est = est'

  -- plot summary of estimated piece 
  putStrLn $ "logpot/event estimate: " ++ show
    (meanLogPotential aopts est model)
  let fsfp    = featuresFp options
      avgFEst = meanFeatCounts aopts est model
  unless (q || null fsfp) $ plotOverFeatures
    fsfp
    "Mean Feature Values (MAP Estimate)"
    mfNames
    avgFEst

  -- plot comparision with corpus data
  when (compareCrp options) $ do
    ps <- corpusPieces
    putStrLn $ "logpot/event corpus: " ++ show
      (meanLogPotentialN aopts ps model)
    unless q $ do
      let pfx      = comparePfx options
      let avgFCorp = meanFeatCountsN aopts ps model
      plotOverFeatures (pfx ++ "corpus_feats.pdf")
                       "Mean Feature Values (Corpus)"
                       mfNames
                       avgFCorp
      let avgFDiff = VU.map log $ VU.zipWith (/) avgFEst avgFCorp
      plotOverFeatures (pfx ++ "relative_feats.pdf")
                       "Mean Feature Values (Estimate/Corpus)"
                       mfNames
                       avgFDiff

  -- view the generated piece
  if null $ outputFp options
    then viewPieceTmp est
    else viewPiece est (outputFp options)
