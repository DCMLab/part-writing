module VoiceLeading.Helpers where

import Data.Machine (run, source, (~>), ProcessT)
import Data.Functor.Identity (Identity)
import qualified Data.Map as M
import Data.List (intercalate)

processList :: Foldable t => t i -> ProcessT Identity i o -> [o]
processList ins p = run $ source ins ~> p

showMap :: (Show k, Show v) => M.Map k v -> String
showMap m = "{" ++ (intercalate ", " pairs) ++ "}"
  where pairs = map (\ (k,v) -> show k ++ ": " ++ show v) (M.toList $ m)
