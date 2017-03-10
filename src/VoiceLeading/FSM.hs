module VoiceLeading.FSM where

import Data.Traversable as T
import Control.Monad as Monad
import Data.Functor.Identity (Identity)
import GHC.Base ((<|>))

import qualified Data.Machine as M

data Mealy i o s = SimpleMealy (s -> i -> (s, o))

data Reduced a = Continue a | Reduced a
data Reducer i o s = Reducer
                     { step :: s -> i -> (s, Reduced [o])
                     , rest :: s -> [o] }

runMealy :: Traversable t => Mealy i o s -> s -> t i -> t o
runMealy (SimpleMealy trans) first ins = snd $ mapAccumL trans first ins

runStutterer :: (MonadPlus m, Traversable t) => Mealy i (m o) s -> s -> t i -> m o
runStutterer (SimpleMealy trans) first ins = msum (snd (mapAccumL trans first ins))

stuttererEx _ x = if odd x then (0, [x,x]) else (0, [x])

processList :: Foldable t => t i -> M.ProcessT Identity i o -> [o]
processList ins p = M.run $ M.source ins M.~> p

runReducer :: Foldable t => Reducer i o s -> s -> t i -> [o]
runReducer (Reducer stp rst) first ins = processList ins ((M.unfoldPlan first pl) M.~> M.asParts)
  where pl s = do
          i <- M.await <|> M.yield (rst s) *> M.stop
          let (nexts, ros) = stp s i
          case ros of
            (Reduced os) -> M.yield os *> M.stop
            (Continue os) -> M.yield os
          pl nexts

reducerEx = Reducer
            { step = (\_ i -> if odd i then (0, Continue [i,i]) else (0, Continue [i]))
            , rest = (\_ -> [])
            }
