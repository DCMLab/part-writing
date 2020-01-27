{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import           VoiceLeading.Automaton         ( AutoOpts(..) )
import           VoiceLeading.Features          ( defaultFeaturesNamed
                                                , nfName
                                                , nfFeature
                                                , runFeaturesOn
                                                )
import           VoiceLeading.Learning          ( trainPCD
                                                , TrainingLogEntry(..)
                                                , neighbor
                                                , stoppingXi
                                                , momentum
                                                -- , adam
                                                )
import           VoiceLeading.Distribution      ( evalModelUnnormLog
                                                , expectedFeatsM
                                                , countFeaturesM
                                                , sumFeaturesM
                                                , ModelParams
                                                , FeatureCounts
                                                , Model(..)
                                                )
import           VoiceLeading.Helpers           ( normU
                                                , RFun(..)
                                                , rFun
                                                , defaultSeed
                                                , linear
                                                )
import           VoiceLeading.Theory            ( loadProfiles
                                                , vectorizeProfiles
                                                , matchChordProfiles
                                                )

import           VoiceLeading.IO.Midi           ( corpusPieces )
import           VoiceLeading.IO.LilyPond       ( viewPieceTmp )
import           VoiceLeading.IO.Model          ( saveModel )
import           VoiceLeading.IO.Plotting       ( plotOverFeatures )

import           Options.Applicative
import           Data.Semigroup                 ( (<>)
                                                , Semigroup(..)
                                                )
import           Control.DeepSeq                ( deepseq )
import           Control.Monad                  ( when )
import qualified Control.Monad.State           as ST
import           Control.Monad.IO.Class         ( liftIO )
import           System.IO                      ( stdout
                                                , hFlush
                                                , Handle
                                                , withFile
                                                , IOMode(..)
                                                )
import           System.Random.MWC              ( initialize )
import           Formatting              hiding ( now )
import           Formatting.Clock               ( timeSpecs )
import           Formatting.ShortFormatters
import           System.Clock
import           Data.Aeson
import           Data.Aeson.Encode.Pretty       ( encodePretty )
import qualified Data.ByteString.Lazy          as B
import           Data.Default
import qualified Data.Vector                   as V
import qualified Data.Vector.Unboxed           as VU
import qualified Data.Function.Memoize         as Mem
import qualified Data.Text                     as T
import           Data.Maybe                     ( fromMaybe )
import           GHC.Generics                   ( Generic )

data Opts = Opts
  { iterations :: Int
  , dataSplit :: Double
  , neighborDist :: Double
  , chainSize :: Int
  , resetRate :: Int
  , fRate :: RFun
  , fRateFast :: RFun
  , fPower :: RFun
  , profileFp :: FilePath
  , modelFp :: FilePath
  , diagramFp :: FilePath
  , gradientFp :: FilePath
  , logFp :: FilePath
  , showChain :: Bool }
  deriving (Show, Read, Eq, Generic)

instance ToJSON Opts where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Opts

opts :: Parser Opts
opts =
  Opts
    <$> argument
          auto
          (  help "number of training iterations"
          <> showDefault
          <> value 100
          <> metavar "iterations"
          )
    <*> option
          auto
          (  long "data-split"
          <> short 's'
          <> help "part of the data to use for training (rest is test data)"
          <> showDefault
          <> value 1
          <> metavar "FLOAT"
          )
    <*> option
          auto
          (  long "neighbor-distance"
          <> short 'z'
          <> help
               "probability of changing each note when creating the neighborhood"
          <> showDefault
          <> value 0.1
          <> metavar "FlOAT"
          )
    <*> option
          auto
          (  long "chain-size"
          <> short 'c'
          <> help "number of pieces in the markov chain"
          <> showDefault
          <> value 1
          <> metavar "INT"
          )
    <*> option
          auto
          (  long "reset-rate"
          <> short 'r'
          <> help "reset chain every r iterations. 0 to disable."
          <> showDefault
          <> value 0
          <> metavar "INT"
          )
    <*> option
          auto
          (  long "learning-rate"
          <> short 'l'
          <> help "the value of the learning rate (over time)"
          <> showDefault
          <> value (Cnst 0.1)
          <> metavar "RFUN"
          )
    <*> option
          auto
          (  long "fast-learning-rate"
          <> short 'f'
          <> help "the learning rate for the fast parameters"
          <> showDefault
          <> value (Cnst 0.1)
          <> metavar "RFUN"
          )
    <*> option
          auto
          (  long "power"
          <> short 'p'
          <> help "the value of the annealing power (over time)"
          <> showDefault
          <> value (Cnst 1)
          <> metavar "RFUN"
          )
    <*> strOption
          (  long "chord-profile"
          <> short 'P'
          <> help "the file from which to read the chord profiles"
          <> showDefault
          <> value "data/jsbach_chorals_harmony/profiles.json"
          <> metavar "FILE"
          )
    <*> strOption
          (  long "model-out"
          <> short 'o'
          <> help "output file for the trained model"
          <> showDefault
          <> value "model.json"
          <> metavar "FILE"
          )
    <*> strOption
          (  long "diagram-out"
          <> short 'd'
          <> help "filename for diagram"
          <> showDefault
          <> value "diagram.pdf"
          <> metavar "FILE"
          )
    <*> strOption
          (  long "gradient-out"
          <> short 'g'
          <> help "filename for gradient"
          <> showDefault
          <> value "gradient.pdf"
          <> metavar "FILE"
          )
    <*> strOption
          (  long "log"
          <> short 'L'
          <> help "filename for log"
          <> showDefault
          <> value ""
          <> metavar "FILE"
          )
    <*> switch
          (long "show-chain" <> help "show the last samples of the PCD chain")

optsInfo :: ParserInfo Opts
optsInfo = info
  (opts <**> helper)
  (fullDesc <> progDesc "Train model parameters from the corpus")

data LogState = LogState { lsStart :: TimeSpec
                         , lsOld :: TimeSpec
                         , lsESMax :: Maybe Double
                         , lsESBest :: Maybe ModelParams }
                deriving (Show)

type LogAction = ST.StateT LogState IO ()
type Logger = TrainingLogEntry -> LogAction

(<+>) :: Logger -> Logger -> Logger
l1 <+> l2 = \entry -> l1 entry >> l2 entry

stdLogger
  :: V.Vector T.Text
  -> FeatureCounts
  -> FeatureCounts
  -> (ModelParams -> Double)
  -> FilePath
  -> FilePath
  -> Logger
stdLogger names train test stopCrit fpParams fpGrad =
  logParams names fpParams
    <+> logGrad names fpGrad
    <+> logShort
    <+> logObjective train test stopCrit
    <+> logBest stopCrit

logParams :: V.Vector T.Text -> FilePath -> Logger
logParams names fp (TLogEntry _ _ params _ _ _ _) =
  ST.lift $ plotOverFeatures fp "Model Feature Weights" names params

logGrad :: V.Vector T.Text -> FilePath -> Logger
logGrad names fp (TLogEntry _ _ _ gradient _ _ _) =
  ST.lift $ plotOverFeatures fp "Current Gradient" names gradient

evalObjective
  :: ModelParams
  -> FeatureCounts
  -> FeatureCounts
  -> FeatureCounts
  -> (Double, Double)
evalObjective params train test chain = (cdTrain, cdTest)
 where
  pChain  = evalModelUnnormLog chain params
  pTrain  = evalModelUnnormLog train params
  pTest   = evalModelUnnormLog test params
  cdTrain = pTrain - pChain
  cdTest  = pTest - pChain

logObjective
  :: FeatureCounts -> FeatureCounts -> (ModelParams -> Double) -> Logger
logObjective train test stopCrit (TLogEntry _ _ params _ _ _ chain) =
  ST.lift
    $  putStrLn
    $  "cdTrain: "
    <> show cdTrain
    <> ", cdTest: "
    <> show cdTest
    <> ", log xi: "
    <> show (stopCrit params)
  where (cdTrain, cdTest) = evalObjective params train test chain

logBest :: (ModelParams -> Double) -> Logger
logBest stopCrit (TLogEntry _ _ params _ _ _ _) = do
  let crit = stopCrit params
  lstate <- ST.get
  when (maybe True (crit >) $ lsESMax lstate) $ ST.put $ lstate
    { lsESMax  = Just crit
    , lsESBest = Just params
    }

logShort :: Logger
logShort (TLogEntry it progr _ gradient power rate _) = do
  now    <- ST.lift $ getTime Monotonic
  lstate <- ST.get
  let old  = lsOld lstate
      diff = diffTimeSpec now old
      lf pad prec = left pad ' ' %. f prec
  ST.put $ lstate { lsOld = now }
  ST.lift $ fprint
    ( (left 4 ' ' %. int)
    % " ("
    % lf 4 2
    % ") ["
    % (left 7 ' ' %. timeSpecs)
    % "]: power = "
    % lf 5 2
    % ", learning rate = "
    % lf 5 2
    % ", |gradient| = "
    % f 5
    % "\n"
    )
    it
    progr
    old
    now
    power
    rate
    (normU gradient)
  -- printf "%4d (%4.2f) [%2d.%.2d]: power = % 5.2f, learning rate = % 5.2f, |gradient| = % 7.5f"

logJSON
  :: FeatureCounts
  -> FeatureCounts
  -> (ModelParams -> Double)
  -> Handle
  -> Logger
logJSON train test stopCrit h (TLogEntry it prog params gradient power rate chain)
  = do
    let obj = object
          [ "iteration" .= it
          , "progress" .= prog
          , "params" .= params
          , "gradient" .= gradient
          , "grad_norm" .= normU gradient
          , "power" .= power
          , "rate" .= rate
          , "cdTrain" .= cdTrain
          , "cdTest" .= cdTest
          , "stopCrit" .= stopCrit params
          ]
    ST.lift $ B.hPut h $ encodePretty obj
  where (cdTrain, cdTest) = evalObjective params train test chain
  -- model = object $ V.toList $ V.zipWith (.=) names (V.convert params)

logJSONHeader :: Handle -> V.Vector T.Text -> Opts -> LogAction
logJSONHeader h names options = ST.lift $ B.hPut h $ encodePretty $ object
  ["features" .= names, "options" .= options]

main :: IO ()
main = do
  -- gen     <- liftIO createSystemRandom
  gen     <- initialize defaultSeed -- for reproducability
  options <- execParser optsInfo
  -- putStrLn $ show options

  let featuresNamed = V.fromList defaultFeaturesNamed
      feats         = nfFeature <$> featuresNamed
      fNames        = nfName <$> featuresNamed
      nFeats        = V.length featuresNamed

  (Just pfs) <- loadProfiles $ profileFp options
  let profiles = vectorizeProfiles pfs
      harmEst  = Mem.memoize $ matchChordProfiles profiles
      aopts    = def { oHarmEstimator = harmEst }

  pieces <- corpusPieces
  let nsplit = round $ fromIntegral (length pieces) * dataSplit options
      (trainPieces, testPieces) = splitAt nsplit pieces
      ntrain = fromIntegral nsplit
  liftIO $ putStr "counting features of corpus... "
  liftIO $ hFlush stdout
  expTrain  <- expectedFeatsM aopts trainPieces feats
  expTest   <- expectedFeatsM aopts testPieces feats

  neighbors <- mapM (neighbor gen $ neighborDist options) pieces
  let countsNbh = concatMap (\nb -> runFeaturesOn aopts nb feats) neighbors
  -- neighbors <- mapM (\d -> mapM (neighbor gen d) pieces)
  --                   [1, 0.5, 0.1, 0.05, 0.01, 0.001]
  -- let countsNbh = concatMap (\nb -> runFeaturesOn aopts nb feats) <$> neighbors
  countsTrainPieces <- mapM (\nb -> countFeaturesM aopts nb feats) trainPieces
  countsTrain <- VU.map (/ ntrain) <$> sumFeaturesM feats countsTrainPieces

  liftIO $ expTrain `deepseq` expTest `deepseq` putStrLn "done."

  let scale    = 0 -- fromIntegral $ maximum $ pieceLen <$> pieces
      stopCrit = stoppingXi scale countsTrain countsNbh
      logger   = stdLogger fNames
                           expTrain
                           expTest
                           stopCrit
                           (diagramFp options)
                           (gradientFp options)
      -- optimizer = adam 0.8 0.998 nFeats
      optimizer = momentum (linear 0.5 0.9) nFeats
      train     = trainPCD optimizer
                           gen
                           aopts
                           trainPieces
                           expTrain
                           defaultFeaturesNamed
                           (iterations options)
                           (chainSize options)
                           (resetRate options)
                           (rFun $ fPower options)
                           (rFun $ fRate options)
                           (rFun $ fRateFast options)
  now <- getTime Monotonic
  let logState0 = LogState now now Nothing Nothing
  ((lastModel, chain), lsFinal) <- if null (logFp options)
    then ST.runStateT (train logger) logState0
    else withFile (logFp options) WriteMode $ \h -> ST.runStateT
      (do
        logJSONHeader h fNames options
        train $ logger <+> logJSON expTrain expTest stopCrit h
      )
      logState0
  let model = maybe lastModel (Model featuresNamed) (lsESBest lsFinal)
  print $ lsESMax lsFinal
  saveModel model "trained model" (show options) (modelFp options)
  when (showChain options) $ mapM_ viewPieceTmp chain
