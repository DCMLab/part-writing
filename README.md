# vl-haskell

A formal model of voice leading rules implemented in [Haskell](https://www.haskell.org/).

## Building

vl-haskell can be built with [stack](https://docs.haskellstack.org/en/stable/README/).
If required, run

    stack setup

and build vl-haskell with

    stack build

which will download all necessary dependencies.
Documentation can be built with

    stack haddock --open

where `--open` opens the documentation index in your default browser.
You can find the documentation for this project under "VoiceLeading".

## Usage

vl-haskell provides two main executables, `vl-train` and `vl-compose`.
`vl-train` takes some learning parameters, runs a training session over
all 4-voiced MIDI files in `data/corpus/`, and writes the resulting trained model
to a json file.
`vl-compose` takes such a model file and composes a piece from it by
searching for a MAP assignment, i.e., a piece with a very high probability.
This can be constraint by starting with a given piece and keeping some voices
(e.g., soprano, or soprano and bass) intact, recomposing only the remaining voices.

Both commands need to be invoked via stack:

    stack exec -- vl-{train,compose}

The `--` is included in order to avoid arguments being passed to `stack exec`.
For a documentation of each tool, use the `-h` (or `--help`) flag.
An already trained model that can be used for `vl-compose` can be found in `exampleModel.json`.

The two additional commands `vl-haskell-exe` and `vl-view-model`
are WIP and should not be used.

## Module Overview

### Basic Modules (`src/VoiceLeading/`)

VoiceLeading.Base - contains the code for representing music as
a sequence of events.

VoiceLeading.Theory - some simple music theory functions.

VoiceLeading.Helpers - helper functions used across the project.

### Functionality Modules (`src/VoiceLeading/`)

VoiceLeading.Automaton - defines the state machine and features
and contains code for running features over a piece.

VoiceLeading.Distribution - defines models that represent a
distribution using features and weights.
Also contains code for evaluating the a piece under a given model.

VoiceLeading.Learning - provides an algorithm based on
Persistent Contrastive Divergence to learn model parameters from a corpus.

VoiceLeading.Inference - provides algorithms to find the
MAP assignment of a distribution (i.e., compose an "optimal" piece).

### IO Modules (`src/VoiceLeading/IO/`)

VoiceLeading.IO.Midi - load pieces from MIDI files.

VoiceLeading.IO.LilyPond - export pieces to LilyPond and view the PDFs.

VoiceLeading.IO.Model - load and save models to and from JSON.

VoiceLeading.IO.Plotting - utilities for plotting values over features,
e.g., model parameters.

VoiceLeading.IO.HorizontalBars - a horizontal bars layout for plotting.

## Code Example

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
