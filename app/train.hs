{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           VoiceLeading.Automaton         ( defaultFeaturesNamed
                                                , AutoOpts(..)
                                                )
import           VoiceLeading.Learning          ( trainPCD
                                                , TrainingLogEntry(..)
                                                )
import           VoiceLeading.Distribution      ( evalModelUnnormLog
                                                , ModelParams
                                                , FeatureCounts
                                                )
import           VoiceLeading.Helpers           ( normU
                                                , RFun(..)
                                                , rFun
                                                )
import           VoiceLeading.Theory            ( loadProfiles
                                                , vectorizeProfiles
                                                , matchChordProfiles
                                                , findHarm
                                                )

import           VoiceLeading.IO.Midi           ( corpusPieces )
import           VoiceLeading.IO.LilyPond       ( viewPieceTmp )
import           VoiceLeading.IO.Model          ( saveModel )
import           VoiceLeading.IO.Plotting       ( plotOverFeatures )

import           Options.Applicative
import           Data.Semigroup                 ( (<>)
                                                , Semigroup(..)
                                                )
import           Control.Monad                  ( unless )
import qualified Control.Monad.State           as ST

--import Text.Printf (printf)
import           System.IO                      ( stdout
                                                , Handle(..)
                                                , withFile
                                                , IOMode(..)
                                                )
import           Formatting
import           Formatting.Clock               ( timeSpecs )
import           Formatting.ShortFormatters
import           System.Clock
import           Data.Aeson
import           Data.Aeson.Encode.Pretty       ( encodePretty )
import qualified Data.ByteString.Lazy          as B
import           Data.Default
import qualified Data.Vector                   as V
import qualified Data.Function.Memoize         as Mem

data Opts = Opts
  { iterations :: Int
  , dataSplit :: Double
  , chainSize :: Int
  , resetRate :: Int
  , fRate :: RFun
  , fPower :: RFun
  , profileFp :: FilePath
  , modelFp :: FilePath
  , diagramFp :: FilePath
  , gradientFp :: FilePath
  , logFp :: FilePath
  , hideChain :: Bool }
  deriving (Show, Read, Eq)

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
          (long "hide-chain" <> short 'q' <> help
            "don't show the last samples of the PCD chain"
          )

optsInfo = info
  (opts <**> helper)
  (fullDesc <> progDesc "Train model parameters from the corpus")

type LogAction = ST.StateT (TimeSpec, TimeSpec) IO ()
type Logger = TrainingLogEntry -> LogAction

instance Monoid LogAction where
  mempty = pure ()

instance Semigroup LogAction where
  (<>) = (>>)

--instance Monoid Logger where
--  mempty = pure ()

stdLogger :: FilePath -> FilePath -> Logger
stdLogger fpParams fpGrad =
  logParams fpParams <> logGrad fpGrad <> logShort <> logObjective

logParams :: FilePath -> Logger
logParams fp (TLogEntry _ _ names params _ _ _ _ _ _) =
  ST.lift $ plotOverFeatures fp "Model Feature Weights" names params

logGrad :: FilePath -> Logger
logGrad fp (TLogEntry _ _ names _ gradient _ _ _ _ _) =
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

logObjective :: Logger
logObjective (TLogEntry _ _ _ params _ _ _ train test chain) =
  ST.lift
    $  putStrLn
    $  "cdTrain: "
    <> show cdTrain
    <> ", cdTest: "
    <> show cdTest
  where (cdTrain, cdTest) = evalObjective params train test chain

logShort :: Logger
logShort (TLogEntry it progr _ _ gradient power rate _ _ _) = do
  now          <- ST.lift $ getTime Monotonic
  (start, old) <- ST.get
  ST.put (start, now)
  let diff = diffTimeSpec now old
      lf pad prec = left pad ' ' %. f prec
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

logJSON :: Handle -> Logger
logJSON h (TLogEntry it prog names params gradient power rate train test chain)
  = do
    now          <- ST.lift $ getTime Monotonic
    (start, old) <- ST.get
    ST.put (start, now)
    let diff a b = toNanoSecs $ diffTimeSpec a b
        ts  = diff start now
        to  = diff old now
        obj = object
          [ "iteration" .= it
          , "progress" .= prog
          , "model" .= model
          , "gradient" .= gradient
          , "grad_norm" .= normU gradient
          , "power" .= power
          , "rate" .= rate
          , "time" .= ts
          , "duration" .= to
          , "cdTrain" .= cdTrain
          , "cdTest" .= cdTest
          ]
    ST.lift $ B.hPut h $ encodePretty obj
 where
  model = object $ V.toList $ V.zipWith (.=) names (V.convert params)
  (cdTrain, cdTest) = evalObjective params train test chain

main :: IO ()
main = do
  options    <- execParser optsInfo
  -- putStrLn $ show options
  pieces     <- corpusPieces
  (Just pfs) <- loadProfiles $ profileFp options
  let profiles = vectorizeProfiles pfs
      harmEst  = Mem.memoize $ matchChordProfiles profiles
      aopts    = def { oHarmEstimator = harmEst }
      logger   = stdLogger (diagramFp options) (gradientFp options)
      train    = trainPCD aopts
                          pieces
                          (dataSplit options)
                          defaultFeaturesNamed
                          (iterations options)
                          (chainSize options)
                          (resetRate options)
                          (rFun $ fPower options)
                          (rFun $ fRate options)
  now                 <- getTime Monotonic
  ((model, chain), _) <- if null (logFp options)
    then ST.runStateT (train logger) (now, now)
    else withFile (logFp options) WriteMode
      $ \h -> ST.runStateT (train $ logger <> logJSON h) (now, now)
  -- print model
  saveModel model "trained model" (show options) (modelFp options)
  unless (hideChain options) $ mapM_ viewPieceTmp chain
