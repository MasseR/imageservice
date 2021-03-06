{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Data.Fingerprint where

import           Codec.Picture
import           Codec.Picture.Extra                (scaleBilinear)
import           Control.Comonad                    (extend, extract)
import           Control.Comonad.Store              (Store, experiment, seek,
                                                     store)
import           Data.Bits
import qualified Data.BKTree                        as BK
import           Data.SafeCopy
import qualified Data.Text                          as T
import           Data.Word                          (Word64)
import           MyPrelude

import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.ToRow

-- For testing
import           Test.QuickCheck.Deriving
import           Test.QuickCheck.Deriving.Modifiers
import           Test.QuickCheck.Modifiers


data Alg = Average | DHash deriving (Read, Show, Generic)

data Fingerprint_0 =
  Fingerprint_0 { imagePath :: String
                , hash      :: !Word64
                }
  deriving stock (Show, Generic)
  deriving Arbitrary via ((ASCIIString, Word64) `Isomorphic` Fingerprint_0)

data Fingerprint_1 =
  Fingerprint_1 { imagePath :: Text
              , hash        :: !Word64
              }
  deriving  stock(Show, Generic)
  deriving Arbitrary via ((ASCIIText, Word64) `Isomorphic` Fingerprint_1)

data Fingerprint =
  Fingerprint { imagePath :: !Text
              , hash      :: !Word64
              , checked   :: Maybe UTCTime
              }
  deriving stock (Show, Generic, Eq)
  deriving Arbitrary via ((ASCIIText, Word64, Maybe (UTCRange '(2020,1,1,0) '(2021,1,1,0))) `Isomorphic` Fingerprint)

instance ToRow Fingerprint where
  toRow Fingerprint{..} = toRow (imagePath, hash, checked)

instance FromRow Fingerprint where
  fromRow = do
    (imagePath, hash, checked) <- fromRow
    pure Fingerprint{..}

deriveSafeCopy 0 'base ''Fingerprint_0
deriveSafeCopy 1 'extension ''Fingerprint_1
deriveSafeCopy 2 'extension ''Fingerprint

instance Migrate Fingerprint_1 where
  type MigrateFrom Fingerprint_1 = Fingerprint_0
  migrate Fingerprint_0{imagePath=p,hash=h} = Fingerprint_1{imagePath=T.pack p, hash=h}

instance Migrate Fingerprint where
  type MigrateFrom Fingerprint = Fingerprint_1
  migrate Fingerprint_1{imagePath=p,hash=h} = Fingerprint{imagePath=p, hash=h, checked = Nothing}

instance BK.Metric Fingerprint where
  -- hamming distance
  distance (Fingerprint _ a _) (Fingerprint _ b _) =
    let xored = a `xor` b
    in foldr (\shiftA acc -> acc + if 1 `shift` shiftA .&. xored > 0 then 1 else 0) 0 [0..63]

fingerprint :: Alg -> DynamicImage -> Word64
fingerprint alg = mkHash . grey . scale . convertRGB8
  where
    scale :: Image PixelRGB8 -> Image PixelRGB8
    scale = scaleBilinear 8 8
    grey :: Image PixelRGB8 -> Image Pixel8
    grey = pixelMap (\(PixelRGB8 r g b) -> ceiling ((fromIntegral r * (0.3 :: Double)) + (fromIntegral g * 0.59) + (fromIntegral b * 0.11)))
    indexes = [(x,y) | x <- [0..7], y <- [0..7]]
    hashWith :: Image Pixel8 -> (Store (Int, Int) Pixel8 -> Bool) -> Word64
    hashWith img f =
      let s = store (uncurry (pixelAt img)) (0,0)
          img' = extend f s
          at pos = extract $ seek pos img'
      in foldr (\(shiftA, b) acc -> if b then 1 `shift` shiftA .|. acc else acc) 0 $ zip [0..] $ map at indexes
    mkHash :: Image Pixel8 -> Word64
    mkHash img = case alg of
                Average ->
                  let avg = fromIntegral (foldl' (\acc (x,y) -> acc + fromIntegral (pixelAt img x y)) (0 :: Int) [(x,y) | (x,y) <- indexes] `div` 64)
                  in img `hashWith` avgAlg avg
                DHash -> img `hashWith` dAlg
    dAlg :: Store (Int, Int) Pixel8 -> Bool
    dAlg img = case experiment (\(x,y) -> [(x,y), (succ x `mod` 8, y)]) img of
                    [l,r] -> l > r
                    _     -> False
    avgAlg :: Pixel8 -> Store (Int, Int) Pixel8 -> Bool
    avgAlg avg img = extract img > avg

