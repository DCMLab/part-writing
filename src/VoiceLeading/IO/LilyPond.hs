{-|
Module      : VoiceLeading.IO.LilyPond
Description : View the Voice Leading representation as sheet music using LilyPond.
Copyright   : (c) Christoph Finkensiep, 2017
License     : MIT
Maintainer  : chfin@freenet.de
Stability   : experimental
Portability : POSIX

-}
module VoiceLeading.IO.LilyPond
  ( pieceToLy
  , viewLy
  , viewPiece
  , viewMidi
  ) where

import VoiceLeading.Base
import VoiceLeading.IO.Midi

import Data.Traversable
import Data.Functor.Identity (Identity)
import GHC.Base ((<|>))
import GHC.Real ((%))
import Data.Fixed (mod')
import Data.List (sort,genericLength)
import Data.Maybe (mapMaybe)

import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>), (-<.>))
import System.Process (callCommand)

import qualified Data.Music.Lilypond as L
import qualified Data.Music.Lilypond.IO as LIO
import qualified Text.Pretty as P

import qualified Data.Machine as MC

-- | Convert a 'Piece' to a LilyPond 'String'.
pieceToLy :: Voice v => Piece v -> String
pieceToLy p@(Piece meta _) = lyHeader meta ++ P.runPrinter (P.pretty (pieceToLy' p))

lyHeader :: PieceMeta -> String
lyHeader meta = "\\version \"2.18.2\"\n"
                ++ "\\header { title = \"" ++ title meta ++ "\""
                ++ " subtitle = \" \""
                ++ " tagline = \"\" }\n"
                ++ "\\paper { indent = 0 }"

pieceToLy' :: Voice v => Piece v -> L.Music
pieceToLy' p@(Piece meta (e1:_)) = L.New "StaffGroup" Nothing $ L.Simultaneous False staves
  where vs     = reverse $ sort (voices e1)
        staves = map (L.New "Staff" Nothing . voiceToLy p) vs

processList :: Foldable t => t i -> MC.ProcessT Identity i o -> [o]
processList ins p = MC.run $ MC.source ins MC.~> p

voiceToLy :: Voice v => Piece v -> v -> L.Music
voiceToLy (Piece meta evs@(e1:_)) v = L.Sequential $ global ++ processList evs converter ++ final
  where (num, denom) = timeSignature meta
        key          = keySignature meta
        barLen       = toRational num
        upBeat       = evBeat e1
        (kp, km)     = guessKey key
        global       = [L.Time num denom, L.Key kp km, L.Clef $ guessClef v evs]
                       -- some way to add \partial?
        final        = [] -- \bar "|." not possible?
        converter    = voiceMachine key barLen (1 % denom) upBeat v

voiceMachine :: Voice v => KeySig -> Beat -> Beat -> Beat -> v
             -> MC.ProcessT Identity (Event v) L.Music
voiceMachine key barLen beatLen upBeat v = MC.unfoldPlan (0,Nothing) go
  where go (lastBeat,lastNote) = do
          ev <- MC.await <|> completeLast lastNote (mkDur upBeat lastBeat) False *> MC.stop
          let note = evGet ev v
              beat = evBeat ev
              dur = mkDur beat lastBeat
          completeLast lastNote dur (pitchHolds note)
          case note of
            Rest      -> go (beat, Just $ L.Rest Nothing [])
            Pitch p _ -> go (beat, Just $ L.Note (midiToNote key p) Nothing [])
        completeLast last dur hold =
          let post = (if hold then [L.Tie] else []) in
          case last of
            Just (L.Rest _ _)   -> MC.yield $ L.Rest dur post
            Just (L.Note p _ _) -> MC.yield $ L.Note p dur post
            _                   -> MC.yield $ L.Rest dur [] -- pure ()
              -- usually yield nothing, but now rest instead of \partial
        mkDur beat lastBeat = Just $ L.Duration (mod' (beat - lastBeat) barLen * beatLen)

guessClef :: (Voice v) => v -> [Event v] -> L.Clef
guessClef v evs = if mean < 60 then L.Bass else L.Treble 
  where mean = (sum $ mapMaybe (pitchMidi . (flip evGet) v) evs) % length evs

guessKey :: KeySig -> (L.Pitch, L.Mode)
guessKey k@(KeySig r 5) = (midiToPitch k r, L.Minor)
guessKey k@(KeySig r m) = (midiToPitch k (r - modal 0 m), L.Major)

lyPitches = [ (L.C,0), (L.C,1), (L.D,0), (L.E,-1)
            , (L.E,0), (L.F,0), (L.F,1), (L.G,0)
            , (L.A,-1), (L.A,0), (L.B,-1), (L.B,0)]

-- TODO choose accidentals closest to key
midiToPitch :: KeySig -> Int -> L.Pitch
midiToPitch (KeySig r m) p = L.Pitch (name,acc,oct)
  where pc         = mod p 12
        (name,acc) = lyPitches !! pc
        oct        = div p 12

midiToNote :: KeySig -> Int -> L.Note
midiToNote k p = L.NotePitch (midiToPitch k p) Nothing

-- IO section
-------------

midiToLy :: FilePath -> IO String
midiToLy fp = do
  piece <- loadMidi fp :: IO (Piece ChoralVoice)
  return $ pieceToLy piece

-- | Compile a LilyPond 'String' and view the resulting PDF file.
--   Both the .ly and .pdf files are saved in a temporary directory
--   and deleted after the PDF viewer is closed.
viewLy :: String -> IO ()
viewLy lystr = withSystemTempDirectory "showly" $ \dir -> do
  let fp    = dir </> "music.ly"
      outfp = dir </> "music"
  writeFile fp lystr
  callCommand $ "lilypond -o " ++ outfp ++ " " ++ fp
  callCommand $ "evince " ++ fp -<.> "pdf"
  putStrLn $ "wrote file: " ++ fp

-- | View a 'Piece' using LilyPond (cf. 'viewLy').
viewPiece :: Voice v => Piece v -> IO ()
viewPiece piece = viewLy $ pieceToLy piece

-- | Load a MIDI file and view its representation using LilyPond (cf. 'viewLy').
viewMidi :: FilePath -> IO ()
viewMidi fp = midiToLy fp >>= viewLy
