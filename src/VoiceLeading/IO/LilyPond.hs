{-|
Module      : VoiceLeading.IO.LilyPond
Description : View the pieces as sheet music using LilyPond.
Copyright   : (c) Christoph Finkensiep, 2017
License     : MIT
Maintainer  : chfin@chfin.de
Stability   : experimental
Portability : Linux (requires xdg-open)

This modules allows the conversion of a 'Piece' to a 'String' in
Lilypond format.
The pure conversion is done by 'pieceToLy'.
Additionally, a range of converter functions exist to save and compile
the LilyPond representation, and to import directly from Midi.
The export functions all have a variant, where output and compilation are done
in a temporary directory.
-}
module VoiceLeading.IO.LilyPond
  ( pieceToLy
  , midiToLy
  , viewLy
  , viewLyTmp
  , viewPiece
  , viewPieceTmp
  , viewMidi
  , viewMidiTmp
  )
where

import           VoiceLeading.Base
import           VoiceLeading.IO.Midi
import           VoiceLeading.Helpers           ( processList )
import           VoiceLeading.Theory            ( modal )

import           Data.Traversable
import           Data.Functor.Identity          ( Identity )
import           GHC.Base                       ( (<|>) )
import           Data.Ratio
import           Data.Fixed                     ( mod' )
import           Data.List                      ( sort
                                                , genericLength
                                                , find
                                                )
import           Data.Maybe                     ( mapMaybe )

import           System.IO.Temp                 ( withSystemTempDirectory )
import           System.FilePath                ( (</>)
                                                , (-<.>)
                                                , dropExtension
                                                )
import           System.Process                 ( callCommand )

import qualified Data.Music.Lilypond           as L
import qualified Data.Music.Lilypond.IO        as LIO
import qualified Text.Pretty                   as P

import qualified Data.Machine                  as MC

-- | Convert a 'Piece' to a LilyPond 'String'.
pieceToLy :: Voice v => Piece v -> String
pieceToLy p@(Piece meta _) =
  lyHeader meta ++ P.runPrinter (P.pretty (pieceToLy' p)) ++ lyFooter

lyHeader :: PieceMeta -> String
lyHeader meta =
  "\\version \"2.18.2\"\n"
    ++ "\\header { title = \""
    ++ title meta
    ++ "\""
    ++ " subtitle = \" \""
    ++ " tagline = \"\" }\n"
    ++ "\\paper { indent = 0 }\n"
    ++ "\\score {\n"

lyFooter :: String
lyFooter = "\n  \\layout{}\n  \\midi{ \\tempo 4 = 90 }\n}"

pieceToLy' :: Voice v => Piece v -> L.Music
pieceToLy' p@(Piece meta (e1 : _)) =
  L.New "StaffGroup" Nothing $ L.Simultaneous False staves
 where
  vs     = reverse $ sort (voices e1)
  staves = map (L.New "Staff" Nothing . voiceToLy p) vs

voiceToLy :: Voice v => Piece v -> v -> L.Music
voiceToLy (Piece meta evs@(e1 : _)) v =
  L.Sequential $ global ++ processList evs converter ++ final
 where
  (num, denom) = timeSignature meta
  key          = keySignature meta
  barLen       = toRational num
  upBeat       = evBeat e1
  (kp, km)     = guessKey key
  global       = [L.Time num denom, L.Key kp km, L.Clef $ guessClef v evs]
                 -- some way to add \partial?
  final        = [] -- \bar "|." not possible?
  converter    = voiceMachine key barLen (1 % denom) upBeat v

voiceMachine
  :: Voice v
  => KeySig
  -> Beat
  -> Beat
  -> Beat
  -> v
  -> MC.ProcessT Identity (Event v) L.Music
voiceMachine key barLen beatLen upBeat v = MC.unfoldPlan (0, Nothing) go
 where
  go (lastBeat, lastNote) = do
    ev <-
      MC.await
      <|> completeLast lastNote (calcDur upBeat lastBeat) False
      *>  MC.stop
    let note = evGet ev v
        beat = evBeat ev
        dur  = calcDur beat lastBeat
    completeLast lastNote dur (pitchHolds note)
    case note of
      Rest      -> go (beat, Just $ L.Rest Nothing [])
      Pitch p _ -> go (beat, Just $ L.Note (midiToNote key p) Nothing [])
  completeLast last dur hold =
    let post = (if hold then [L.Tie] else [])
    in  case last of
          Just (L.Rest _ _) -> yieldNote L.Rest dur post
          Just (L.Note p _ _) -> yieldNote (L.Note p) dur post
          _ -> if wholeBar dur then pure () else yieldNote L.Rest dur []
        -- usually yield nothing, but now rest instead of \partial
  calcDur beat lastBeat = (mod' (beat - lastBeat) barLen * beatLen)
  mkDur dur = Just $ L.Duration (if dur == 0 then barLen * beatLen else dur)
  wholeBar dur = dur == (barLen * beatLen)
  po2 n = elem n $ map (2 ^) [0 .. n]
  po2' n = elem n $ map (\i -> (2 ^ i) - 1) [0 .. n]
  fracPart f = maybe 1 id $ find (\i -> 1 % (2 ^ i) < f) [0 ..]
  yieldNote note dur post
    | po2' (numerator dur) = MC.yield $ note (mkDur dur) post
    | otherwise = do
      case (note Nothing []) of
        (L.Rest _ _  ) -> MC.yield $ note (mkDur first) []
        (L.Note _ _ _) -> MC.yield $ note (mkDur first) [L.Tie]
      yieldNote note (dur - first) post
    where first = 1 % 2 ^ fracPart dur


guessClef :: (Voice v) => v -> [Event v] -> L.Clef
guessClef v evs = if mean < 60 then L.Bass else L.Treble
 where
  mean =
    (sum $ map (maybe 60 id . pitchMidi . (flip evGet) v) evs) % length evs

guessKey :: KeySig -> (L.Pitch, L.Mode)
guessKey k@(KeySig r 5) = (midiToPitch k r, L.Minor)
guessKey k@(KeySig r m) = (midiToPitch k (r - modal 0 m), L.Major)

lyPitches =
  [ (L.C, 0)
  , (L.C, 1)
  , (L.D, 0)
  , (L.E, -1)
  , (L.E, 0)
  , (L.F, 0)
  , (L.F, 1)
  , (L.G, 0)
  , (L.A, -1)
  , (L.A, 0)
  , (L.B, -1)
  , (L.B, 0)
  ]

-- TODO choose accidentals closest to key
midiToPitch :: KeySig -> Int -> L.Pitch
midiToPitch (KeySig r m) p = L.Pitch (name, acc, oct)
 where
  pc          = mod p 12
  (name, acc) = lyPitches !! pc
  oct         = div p 12

midiToNote :: KeySig -> Int -> L.Note
midiToNote k p = L.NotePitch (midiToPitch k p) Nothing

-- IO section
-------------

-- | Loads a Midi file and returns a LilyPond string.
midiToLy :: FilePath -> IO String
midiToLy fp = do
  piece <- loadMidi fp :: IO (Piece ChoralVoice)
  return $ pieceToLy piece

-- | Compile a LilyPond 'String' and view the resulting PDF file.
--   Both the .ly and .pdf files are saved in a temporary directory
--   and deleted after the PDF viewer is closed.
viewLyTmp :: String -> IO ()
viewLyTmp lystr = withSystemTempDirectory "showly" $ \dir -> do
  let fp = dir </> "music.ly"
  viewLy lystr fp
  putStrLn "press enter to continue (will delete temporary lilypond file)"
  x <- getLine
  pure ()

-- | Compile a LilyPond 'String' and view the resulting PDF file.
viewLy :: String -> FilePath -> IO ()
viewLy lystr fp = do
  let fly = fp -<.> "ly"
  writeFile fp lystr
  callCommand $ "lilypond -o " ++ dropExtension fp ++ " " ++ fly
  callCommand $ "xdg-open " ++ fp -<.> "pdf"
  putStrLn $ "wrote file: " ++ fly

-- | View a 'Piece' using LilyPond (cf. 'viewLy').
viewPiece :: Voice v => Piece v -> FilePath -> IO ()
viewPiece piece = viewLy (pieceToLy piece)

-- | View a 'Piece' using LilyPond (cf. 'viewLyTmp').
--   Uses a temporary directory.
viewPieceTmp :: Voice v => Piece v -> IO ()
viewPieceTmp piece = viewLyTmp $ pieceToLy piece

-- | Load a MIDI file and view its representation using LilyPond (cf. 'viewLy').
viewMidi :: FilePath -> FilePath -> IO ()
viewMidi fin fout = midiToLy fin >>= (flip viewLy) fout

-- | Load a MIDI file and view its representation using LilyPond (cf. 'viewLyTmp').
--   Uses a temporary directory.
viewMidiTmp :: FilePath -> IO ()
viewMidiTmp fin = midiToLy fin >>= viewLyTmp
