module Main where

import VoiceLeading.Base
import VoiceLeading.Automaton (nfName)
import VoiceLeading.Inference ( estimateGibbsAnnealing, uniformRandomPiece'
                              , mapEstimateNotewise)
import VoiceLeading.Distribution ( modelFeatures, loadModel
                                 , meanLogPotential, meanLogPotentialN
                                 , meanFeatCounts, meanFeatCountsN)
import VoiceLeading.Helpers (RFun(..), rFun)
import VoiceLeading.IO.Midi (corpusDir, corpusPieces, testPiece, loadMidi)
import VoiceLeading.IO.LilyPond (viewPiece, viewPieceTmp)
import VoiceLeading.IO.Plotting

import Options.Applicative
import Data.Semigroup ((<>))
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

opts :: Parser Opts
opts = Opts
  <$> argument auto
  ( help "number of estimation iterations"
    <> showDefault
    <> value 20
    <> metavar "iterations" )
  <*> option auto
  ( long "start-with"
    <> short 's'
    <> help "initializes the estimation procedure"
    <> showDefault
    <> value TestPiece
    <> metavar "TestPiece | \"NewPiece INT\" | \"CorpusPiece NAME\" | \"File FILE\"")
  <*> option auto
  ( long "keep-voices"
    <> short 'k'
    <> help "keep these voices from the initial piece (comma separated in \"[]\")"
    <> showDefault
    <> value []
    <> metavar "\"[(Soprano|Alto|Tenor|Bass)*]\"")
  <*> option auto
  ( long "power"
    <> short 'p'
    <> help "the value of the annealing power (over time)"
    <> showDefault
    <> value (Cnst 5)
    <> metavar "RFUN")
  <*> strOption
  ( long "model"
    <> short 'm'
    <> help "model file used for estimation"
    <> showDefault
    <> value "model.json"
    <> metavar "FILE" )
  <*> strOption
  ( long "output"
    <> short 'o'
    <> help "LilyPond filename of final composition. If empty, a temporary file will be used."
    <> showDefault
    <> value ""
    <> metavar "FILE" )
  <*> strOption
  ( long "diagram-out"
    <> short 'd'
    <> help "filename for the model diagram"
    <> showDefault
    <> value "diagram.pdf"
    <> metavar "FILE" )
  <*> strOption
  ( long "feature-summary-out"
    <> short 'f'
    <> help "filename for the feature summary diagram"
    <> showDefault
    <> value "estimate_feats.pdf"
    <> metavar "FILE" )
  <*> switch
  ( long "compare-corpus" <> short 'c' <> help "plot a comparison of the piece with corpus data" )
  <*> strOption
  ( long "compare-corpus-prefix"
    <> showDefault
    <> value ""
    <> help "corpus comparison filenames use the prefix PREFIX"
    <> metavar "PREFIX")
    <*> switch
  ( long "no-plots" <> short 'q' <> help "don't writey plot files")

optsInfo = info (opts <**> helper)
  (fullDesc <> progDesc "Compose a piece by MAP estimation")

main :: IO ()
main = do
  -- read options
  options <- execParser optsInfo
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
  est' <- estimateGibbsAnnealing piece model
          (keepVoices options) (iterations options)
          (rFun $ fPower options)
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
