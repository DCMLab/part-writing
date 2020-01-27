module Main where

import           VoiceLeading.Base
import           VoiceLeading.Distribution      ( Model(..) )

import           VoiceLeading.IO.Model          ( loadModel )
import           VoiceLeading.IO.Plotting       ( plottingLogger )
import           VoiceLeading.IO.LilyPond       ( pieceToLy )
import           VoiceLeading.IO.Midi           ( loadMidi )

import           System.FilePath                ( takeExtension )
import           Options.Applicative
import           Data.Semigroup                 ( (<>) )

data Opts = Opts
          { inputFp  :: FilePath
          , outputFp :: FilePath
          , stdOut   :: Bool
          , tmpOut   :: Bool
          , quiet   :: Bool }

opts :: Parser Opts
opts =
  Opts
    <$> strArgument (help "input filename" <> metavar "INPUT")
    <*> strArgument (help "output filename" <> metavar "OUTPUT")
    <*> switch
          (long "stdout" <> short 's' <> help
            "write output to stdout (implies -q, only for lilypond)"
          )
    <*> switch (long "tmp" <> short 't' <> help "write to temporary file")
    <*> switch (long "quiet" <> short 'q' <> help "don't view the output file")

optsInfo :: ParserInfo Opts
optsInfo = info
  (opts <**> helper)
  (fullDesc <> progDesc "Convert and view a piece/model/MIDI file.")

main :: IO ()
main = do
  options <- execParser optsInfo
  doc     <- loadDoc $ inputFp options
  exportDoc (outputFp options) doc


data Document v = DocPiece (Piece v)
                | DocModel (Model v)

loadDoc :: FilePath -> IO (Document ChoralVoice)
loadDoc fp | takeExtension fp == ".midi" = DocPiece <$> loadMidi fp
           | takeExtension fp == ".json" = DocModel <$> loadModel fp
           | otherwise = error $ "Unrecognized file type " <> takeExtension fp

exportDoc :: Voice v => FilePath -> Document v -> IO ()
exportDoc fp (DocPiece p) = writeFile fp $ pieceToLy p
exportDoc fp (DocModel m) = plottingLogger fp m
