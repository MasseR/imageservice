{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
module Main where

import           Codec.Picture
import           Codec.Picture.Extra (scaleBilinear)
import           Control.Exception   (SomeException, try)
import           Control.Monad       (forM_, void, when)
import           Data.Bits
import qualified Data.BKTree         as BK
import           Data.List           (foldl')
import           Data.Maybe          (fromMaybe)
import           Data.Word           (Word64)
import           Options.Generic
import           Pipes
import           Pipes.Files
import qualified Pipes.Prelude       as P
import           Pipes.Safe          (runSafeT)
import           System.Directory    (createDirectoryIfMissing, createFileLink)
import           System.FilePath     (takeFileName, (</>))


data Cmd = Cmd { source :: FilePath
               , target :: FilePath
               , range  :: Maybe Int
               } deriving (Show, Generic, ParseRecord)

data Fingerprint =
  Fingerprint { imagePath :: FilePath
              , hash      :: !Word64
              } deriving Show

instance BK.Metric Fingerprint where
  -- hamming distance
  distance (Fingerprint _ a) (Fingerprint _ b) =
    let xored = a `xor` b
    in foldr (\shiftA acc -> acc + if 1 `shift` shiftA .&. xored > 0 then 1 else 0) 0 [0..63]


fingerprint :: DynamicImage -> Word64
fingerprint = hash . grey . scale . convertRGB8
  where
    scale :: Image PixelRGB8 -> Image PixelRGB8
    scale = scaleBilinear 8 8
    grey :: Image PixelRGB8 -> Image Pixel8
    grey = pixelMap (\(PixelRGB8 r g b) -> ceiling ((fromIntegral r * (0.3 :: Double)) + (fromIntegral g * 0.59) + (fromIntegral b * 0.11)))
    hash :: Image Pixel8 -> Word64
    hash img = -- the average fingerprint method
      let avg = fromIntegral (foldl' (\acc (x,y) -> acc + fromIntegral (pixelAt img x y)) (0 :: Int) [(x,y) | x <- [0..7], y <- [0..7]] `div` 64)
      in foldr (\(shiftA, b) acc -> if b then 1 `shift` shiftA .|. acc else acc) 0 $ zip [0..] [pixelAt img x y > avg | x <- [0..7], y <- [0..7]]

main :: IO ()
main = do
  Cmd{..} <- getRecord "Image duplicate finder"
  -- XXX: This is a really long line, split it up
  index <- runSafeT (P.fold (\acc -> either (const acc) (\x -> x `seq` BK.insert x acc)) BK.empty id (find source (glob "*.jpg" <> regular) >-> P.mapM (\path -> liftIO (putStrLn path) >> fmap (path,) <$> liftIO (readImage path)) >-> P.map (fmap (\(path, img) -> Fingerprint path (fingerprint img)))))
  forM_ index $ \fp -> do
    let similar = BK.search (fromMaybe 1 range) fp index
    when (length similar > 1) $ do
      print similar
      let targetDir = target </> show (hash fp)
      createDirectoryIfMissing True targetDir
      forM_ similar $ \fp' ->
        void $ try @SomeException (createFileLink (imagePath fp') (targetDir </> takeFileName (imagePath fp')))
