{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings   #-}
module Network.HTTP.Images.Common
  ( imgAlgebra
  , cata
  ) where

import           Data.Functor.Foldable
import           MyPrelude
import           Network.HTTP.Images.Types
import qualified Data.Text as T

type Alg f a = f a -> a

imgAlgebra :: Alg (ListF (Maybe Text)) [Href]
imgAlgebra = \case
  Cons (Just x) xs -> sorter x : xs
  Cons Nothing xs -> xs
  Nil -> []
  where
    imageSuffix :: Text -> Bool
    imageSuffix x = suffix x `member` (["jpg", "png"] :: Set Text)
    suffix = T.reverse . T.takeWhile (/= '.') . T.reverse
    sorter :: Text -> Href
    sorter url
      | "imgur.com/a/" `T.isInfixOf` url = ImgurAlbum url
      | imageSuffix url = RawImg url
      | otherwise = Html url


