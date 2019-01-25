{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
module Data.Fingerprint where

import           Codec.Picture
import           Codec.Picture.Extra   (scaleBilinear)
import           Control.Comonad       (extend, extract)
import           Control.Comonad.Store (Store, experiment, seek, store)
import           Control.Exception     (SomeException, try)
import           Control.Monad         (forM_, void, when)
import           Data.Bits
import qualified Data.BKTree           as BK
import           Data.List             (foldl')
import           Data.Maybe            (fromMaybe)
import           Data.Word             (Word64)
import           Options.Generic
import           Pipes
import           Pipes.Files
import qualified Pipes.Prelude         as P
import           Pipes.Safe            (runSafeT)
import           System.Directory      (createDirectoryIfMissing,
                                        createFileLink)
import           System.FilePath       (takeFileName, (</>))


data Alg = Average | DHash deriving (Read, Show, Generic)

instance ParseField Alg

data Cmd = Cmd { source    :: FilePath
               , target    :: FilePath
               , range     :: Maybe Int
               , algorithm :: Maybe Alg
               } deriving (Show, Generic, ParseRecord)

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
fingerprint alg = hash . grey . scale . convertRGB8
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
    hash :: Image Pixel8 -> Word64
    hash img = case alg of
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

main :: IO ()
main = do
  Cmd{..} <- getRecord "Image duplicate finder"
  index <- runSafeT (P.fold foldTree BK.empty id (find source (glob "*.jpg" <> regular)
    >-> P.mapM readImg
    >-> P.map (fmap (toFingerprint (fromMaybe Average algorithm)))))
  forM_ index $ \fp -> do
    let similar = BK.search (fromMaybe 1 range) fp index
    when (length similar > 1) $ do
      print similar
      let targetDir = target </> show (hash fp)
      createDirectoryIfMissing True targetDir
      forM_ similar $ \fp' ->
        void $ try @SomeException (createFileLink (imagePath fp') (targetDir </> takeFileName (imagePath fp')))
  where
    readImg path = liftIO (putStrLn path >> fmap (path,) <$> readImage path)
    foldTree acc = either (const acc) (\x -> x `seq` BK.insert x acc)
    toFingerprint alg (path,img) = Fingerprint path (fingerprint alg img)
