module VoiceLeading.Helpers where

import Data.Machine (run, source, (~>), ProcessT)
import Data.Functor.Identity (Identity)
import qualified Data.Map.Strict as M
import Data.List (intercalate)
import Control.Monad.Trans.Maybe
import System.Random.MWC (uniformR, GenIO)

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

-- | Safely replace the head of a list.
replaceHead :: [a] -> (a -> a) -> [a]
replaceHead [] _ = []
replaceHead (x:xs) f = (f x):xs

safeInit :: [a] -> [a]
safeInit [] = []
safeInit lst = init lst

liftMaybe :: (Monad m) => Maybe a -> MaybeT m a
liftMaybe = MaybeT . return

iterateM :: Monad m => Int -> (a -> m a) -> a -> m a
iterateM 0 _ a = return a
iterateM n f a = f a >>= iterateM (n-1) f

norm :: (Foldable f, Functor f, Floating a)  => f a -> a
norm vector = sqrt $ sum $ fmap (\x -> x * x) vector

rotate :: Int -> [a] -> [a]
rotate n xs = take l (drop (n `mod` l) (xs ++ xs))
  where l = length xs

chooseRandom :: [a] -> GenIO -> IO a
chooseRandom lst gen = do
  i <- uniformR (0, length lst - 1) gen
  pure $ lst !! i

--------------------
-- rate functions --
--------------------

linear :: Double -> Double -> Double -> Double
linear start end prog = start + (end - start) * prog

quadratic :: Double -> Double -> Double -> Double
quadratic start end prog = start + (end - start) * prog * prog

power :: Double -> Double -> Double -> Double -> Double
power exponent start end prog = start + (end - start) * (prog ** exponent)

cubic :: Double -> Double -> Double -> Double
cubic = power 3

exponential :: Double -> Double -> Double -> Double
exponential start end prog = start * exp (k * prog)
  where k = log (end / start)

data RFun = Cnst Double
          | Lin Double Double
          | Quad Double Double
          | Cub Double Double
          | Pow Double Double Double
          | Exp Double Double
  deriving (Eq, Show, Read)

rFun :: RFun -> Double -> Double
rFun (Cnst k) = const k
rFun (Lin s e) = linear s e
rFun (Quad s e) = quadratic s e
rFun (Cub s e) = cubic s e
rFun (Pow p s e) = power p s e
rFun (Exp s e) = exponential s e
