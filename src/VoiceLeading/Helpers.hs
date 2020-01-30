{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module VoiceLeading.Helpers where

import           Data.Machine                   ( run
                                                , source
                                                , (~>)
                                                , ProcessT
                                                )
import           Data.Functor.Identity          ( Identity )
import qualified Data.Map.Strict               as M
import           Data.List                      ( intercalate )
import qualified Data.Vector.Unboxed           as VU
import           Control.Monad.Trans.Maybe
import           System.Random.MWC              ( uniformR
                                                , Gen
                                                , GenIO
                                                , createSystemRandom
                                                , save
                                                , fromSeed
                                                , initialize
                                                )
import           Control.Monad.Primitive        ( PrimMonad
                                                , PrimState
                                                )
import           Control.Monad                  ( replicateM_ )
import           Data.Word                      ( Word32 )
import           GHC.Generics                   ( Generic )
import           Data.Aeson
import           Data.Aeson.Types               ( Parser
                                                , typeMismatch
                                                )
import qualified Data.Text                     as T
import qualified Text.ParserCombinators.ReadP  as R
import           System.IO                      ( withFile
                                                , IOMode(..)
                                                , hPrint
                                                )

processList :: Foldable t => t i -> ProcessT Identity i o -> [o]
processList ins p = run $ source ins ~> p

showMap :: (Show k, Show v) => M.Map k v -> String
showMap m = "{" <> intercalate ", " kvPairs <> "}"
  where kvPairs = (\(k, v) -> show k <> ": " <> show v) <$> M.toList m

-- | Safe list access.
-- Negative indices count from the end.
lGet :: [a] -> Int -> Maybe a
lst `lGet` i = g
 where
  l = length lst
  g | i >= 0 && i < l    = Just $ lst !! i
    | i < 0 && (-i) <= l = Just $ lst !! (l - i)
    | otherwise          = Nothing

-- | Safely replace the head of a list.
replaceHead :: [a] -> (a -> a) -> [a]
replaceHead []       _ = []
replaceHead (x : xs) f = f x : xs

safeInit :: [a] -> [a]
safeInit []  = []
safeInit lst = init lst

liftMaybe :: (Monad m) => Maybe a -> MaybeT m a
liftMaybe = MaybeT . return

iterateM :: Monad m => Int -> (a -> m a) -> a -> m a
iterateM 0 _ a = return a
iterateM n f a = f a >>= iterateM (n - 1) f

norm :: (Foldable f, Functor f, Floating a) => f a -> a
norm vector = sqrt $ sum $ fmap (\x -> x * x) vector

normU :: (Floating a, VU.Unbox a) => VU.Vector a -> a
normU vector = sqrt $ VU.sum $ VU.map (\x -> x * x) vector

rotate :: Int -> [a] -> [a]
rotate n xs = take l (drop (n `mod` l) (xs ++ xs)) where l = length xs

chooseRandom :: (PrimMonad m) => [a] -> Gen (PrimState m) -> m a
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
power p start end prog = start + (end - start) * (prog ** p)

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
  deriving (Eq, Show, Read, Generic)

instance ToJSON RFun where
  toJSON = Data.Aeson.String . T.pack . show

parseRead :: Read a => String -> Value -> Parser a
parseRead expected = withText expected $ \v -> pure (read (T.unpack v))

instance FromJSON RFun where
  parseJSON = parseRead "RFun"

rFun :: RFun -> Double -> Double
rFun (Cnst k   ) = const k
rFun (Lin  s e ) = linear s e
rFun (Quad s e ) = quadratic s e
rFun (Cub  s e ) = cubic s e
rFun (Pow p s e) = power p s e
rFun (Exp s e  ) = exponential s e

-----------------
-- random seed --
-----------------

createSeed :: IO (VU.Vector Word32)
createSeed = fromSeed <$> (createSystemRandom >>= save)

writeSeeds :: FilePath -> Int -> IO ()
writeSeeds fp n =
  withFile fp WriteMode $ \file -> replicateM_ n $ createSeed >>= hPrint file

data RandomSeedSpec = DefaultSeed
                    | SystemRandom
                    | Seed (VU.Vector Word32)
  deriving (Eq, Ord)

instance Show RandomSeedSpec where
  show DefaultSeed  = "default"
  show SystemRandom = "random"
  show (Seed s)     = show $ VU.toList s

parseRandomSeedSpec :: R.ReadP RandomSeedSpec
parseRandomSeedSpec =
  (R.string "default" >> R.eof >> return DefaultSeed)
    R.<++ (R.string "random" >> R.eof >> return SystemRandom)
    R.<++ (Seed . VU.fromList <$> R.readS_to_P reads)

instance Read RandomSeedSpec where
  readsPrec _ = R.readP_to_S parseRandomSeedSpec

instance ToJSON RandomSeedSpec where
  toJSON DefaultSeed  = String "default"
  toJSON SystemRandom = String "random"
  toJSON (Seed s)     = toJSON s

instance FromJSON RandomSeedSpec where
  parseJSON arr@(Array  a        ) = Seed . VU.fromList <$> parseJSON arr
  parseJSON (    String "default") = return DefaultSeed
  parseJSON (    String "random" ) = return SystemRandom
  parseJSON invalid                = typeMismatch "RandomSeedSpec" invalid

genFromSeed :: RandomSeedSpec -> IO GenIO
genFromSeed DefaultSeed  = initialize defaultSeed
genFromSeed SystemRandom = createSystemRandom
genFromSeed (Seed s)     = initialize s

defaultSeed :: VU.Vector Word32
defaultSeed = VU.fromList
  [ 1494297517
  , 2325173103
  , 502627107
  , 1342509643
  , 2911293065
  , 144241378
  , 2555216215
  , 3884991836
  , 4054444356
  , 2204934631
  , 3792650103
  , 1841180930
  , 3629627928
  , 3229592185
  , 913676927
  , 2344436190
  , 2095329818
  , 3820652412
  , 4164845697
  , 1253399929
  , 3422500557
  , 4188350959
  , 2129734444
  , 329374895
  , 2448382914
  , 407376883
  , 41625477
  , 2266700027
  , 3951061243
  , 1017345878
  , 3685569254
  , 45601120
  , 1102616899
  , 304143791
  , 355974483
  , 3707002818
  , 614224292
  , 1176519211
  , 3079962272
  , 3138566133
  , 2300032508
  , 1460413152
  , 4221092276
  , 1339421477
  , 3604355546
  , 670382758
  , 1754033092
  , 1912772985
  , 2442109071
  , 2360357211
  , 3860155433
  , 1687582279
  , 1439090272
  , 8145552
  , 4094156166
  , 33401569
  , 1915589787
  , 1250059868
  , 1311993304
  , 2161327039
  , 2582272373
  , 291183483
  , 3274385796
  , 1730666641
  , 4187056417
  , 3970864766
  , 2772136621
  , 1518796699
  , 3772940593
  , 2915902240
  , 1045950184
  , 449317876
  , 3735870137
  , 668890464
  , 3725071999
  , 3541038219
  , 1887059521
  , 1946168251
  , 2780284513
  , 1277948560
  , 1191758467
  , 713085863
  , 259601496
  , 159343403
  , 3307689906
  , 2753655824
  , 1035500846
  , 2527442575
  , 660263056
  , 2408010487
  , 1126735547
  , 1677441487
  , 3453982194
  , 4115360558
  , 2438231540
  , 3442749749
  , 430257840
  , 950466331
  , 201901244
  , 72817290
  , 664219934
  , 3633414083
  , 4135615685
  , 1117145855
  , 1914742881
  , 1417311016
  , 3450135306
  , 2084335748
  , 250465968
  , 4032413600
  , 1244045723
  , 2395190655
  , 2199633238
  , 494927602
  , 3754722235
  , 1866292363
  , 1786450122
  , 2454755403
  , 178255421
  , 1528261350
  , 439281695
  , 3206134799
  , 1632489768
  , 3015573250
  , 1874285171
  , 1509358445
  , 871856666
  , 312098111
  , 2511945868
  , 626922690
  , 3516115894
  , 4100475128
  , 591515512
  , 2813711653
  , 1470816649
  , 3056643911
  , 895710613
  , 3739911659
  , 1140666250
  , 626516908
  , 2184489662
  , 3177379775
  , 597492387
  , 1019050428
  , 1582980062
  , 2505113333
  , 1809212192
  , 1274143942
  , 3106005539
  , 3282071943
  , 2217989839
  , 660781619
  , 2599833031
  , 311479473
  , 1305140147
  , 2968784845
  , 3277493164
  , 2707830805
  , 2639154299
  , 3507304815
  , 2984103786
  , 2104171568
  , 2508531843
  , 2078388800
  , 1098810623
  , 1206782473
  , 759340825
  , 1885090758
  , 1049249185
  , 1301287779
  , 3704747527
  , 4105227966
  , 3037184988
  , 2303824473
  , 1773162163
  , 1705262564
  , 2794966141
  , 1518667083
  , 313490393
  , 2627406083
  , 714611573
  , 1593141351
  , 4258864782
  , 1072795550
  , 2098706399
  , 699238564
  , 432495686
  , 3714173858
  , 628245549
  , 3759380086
  , 2777649153
  , 1530629086
  , 820438462
  , 3749874760
  , 2942178081
  , 2868739678
  , 1026563967
  , 3738184084
  , 975705396
  , 908090994
  , 3282815093
  , 3759515778
  , 2162185464
  , 1116433814
  , 1439233012
  , 2291709051
  , 3142364918
  , 1126998898
  , 3999252702
  , 2648308834
  , 3225579544
  , 1844299146
  , 3004213034
  , 3904872641
  , 2687741955
  , 2192902793
  , 3221057843
  , 2685347588
  , 3966158021
  , 1090151650
  , 2631703499
  , 3920619395
  , 1839346730
  , 2224327703
  , 1724218610
  , 365644964
  , 2496945141
  , 3523142253
  , 4255526632
  , 1741429747
  , 3549702293
  , 2656327181
  , 2418805880
  , 1459135169
  , 2620691741
  , 3912804857
  , 2142247574
  , 3215439677
  , 1986134069
  , 4233186269
  , 1886118860
  , 2676862900
  , 1509186887
  , 1647834088
  , 2355381659
  , 3919783024
  , 4229569175
  , 963162249
  , 1520612245
  , 3610212943
  , 4098674135
  , 75678886
  , 2819383290
  , 203040520
  , 1328117385
  , 656514171
  , 255
  , 362436
  ]
