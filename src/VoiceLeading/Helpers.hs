module VoiceLeading.Helpers where

import Data.Machine (run, source, (~>), ProcessT)
import Data.Functor.Identity (Identity)
import qualified Data.Map.Strict as M
import Data.List (intercalate)

processList :: Foldable t => t i -> ProcessT Identity i o -> [o]
processList ins p = run $ source ins ~> p

showMap :: (Show k, Show v) => M.Map k v -> String
showMap m = "{" ++ (intercalate ", " pairs) ++ "}"
  where pairs = map (\ (k,v) -> show k ++ ": " ++ show v) (M.toList $ m)

-- | Safe list access.
-- Negative indices count from the end.
lGet :: [a] -> Int -> Maybe a
lst `lGet` i = g lst i
  where l = length lst
        g lst i
          | i >= 0 && i < l    = Just $ lst !! i
          | i < 0 && (-i) <= l = Just $ lst !! (l-i)
          | otherwise          = Nothing
