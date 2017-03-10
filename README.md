# vl-haskell

Code for experimenting with voice leading rules written in [Haskell](https://www.haskell.org/).

## Building

vl-haskell can be built with [stack](https://docs.haskellstack.org/en/stable/README/).
If required, run

    stack setup

and build vl-haskell with

    stack build

which will download all necessary dependencies.

## Example

Look at `app/Main.hs` for an example of using the library.
It can be executed by running `stack exec vl-haskell-exe`.

Another (possibly the same) example is given here:
```haskell
import VoiceLeading.Base
import VoiceLeading.IO.Midi
import VoiceLeading.IO.Lilypond

main :: IO ()
main = do
  -- load a piece from Midi
  p <- loadMidi "01AusmeinesHerz.mid" :: IO (Piece ChoralVoice)
  -- give it a nicer title
  let (Piece meta events) = p
      piece = Piece (meta { title = "Aus meines Herzens Grunde" }) events
  -- do something
  viewPiece piece -- shows the internal representation of a piece as notes
```
It shows how
* a piece can be loaded from a MIDI file (see `src/VoiceLeading/MIDI.hs`)
* a the internal representation of a piece can be visualized using
  [LilyPond](http://lilypond.org/) (see `src/VoiceLeading/LilyPond.hs`).

## Documentation

TODO: haddock?
