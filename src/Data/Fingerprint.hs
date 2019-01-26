{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
module Data.Fingerprint where

import           ClassyPrelude
import           Codec.Picture
import           Codec.Picture.Extra   (scaleBilinear)
import           Control.Comonad       (extend, extract)
import           Control.Comonad.Store (Store, experiment, seek, store)
import           Data.Bits
import qualified Data.BKTree           as BK
import           Data.Word             (Word64)


data Alg = Average | DHash deriving (Read, Show, Generic)

data Fingerprint =
  Fingerprint { imagePath :: String
              , hash      :: !Word64
              } deriving (Show, Generic)

instance BK.Metric Fingerprint where
  -- hamming distance
  distance (Fingerprint _ a) (Fingerprint _ b) =
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
                  in img `hashWith` (avgAlg avg)
                DHash -> img `hashWith` dAlg
    dAlg :: Store (Int, Int) Pixel8 -> Bool
    dAlg img = case experiment (\(x,y) -> [(x,y), (succ x `mod` 8, y)]) img of
                    [l,r] -> l > r
                    _     -> False
    avgAlg :: Pixel8 -> Store (Int, Int) Pixel8 -> Bool
    avgAlg avg img = extract img > avg

