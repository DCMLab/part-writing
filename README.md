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
import VoiceLeading.Load
import VoiceLeading.Prob

main :: IO ()
main = do
  pieces <- Load.loadPieces "chorales.json"
  putStrLn  (show (learnAll1 pieces :: ProductOfExperts))
```
It shows how
* a list of pieces can be loaded from a JSON file (see `src/VoiceLeading/Load.hs`)
* a Product of Experts can be learned from the pieces (see `src/VoiceLeading/Prob.hs`).

## Documentation

TODO: haddock?
