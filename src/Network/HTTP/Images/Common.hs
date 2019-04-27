{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
module Network.HTTP.Images.Common
  ( imgAlgebra
  , cata
  ) where

import           Data.Functor.Foldable
import ClassyPrelude
import Network.HTTP.Images.Types

type Alg f a = f a -> a

imgAlgebra :: Alg (ListF (Maybe Text)) [Href]
imgAlgebra = \case
  Cons (Just x) xs -> sorter (unpack x) : xs
  Cons Nothing xs -> xs
  Nil -> []
  where
    imageSuffix :: String -> Bool
    imageSuffix x = suffix x `member` (["jpg", "png"] :: Set String)
    suffix = reverse . takeWhile (/= '.') . reverse
    sorter :: String -> Href
    sorter url
      | "imgur/a/" `isInfixOf` url = ImgurAlbum url
      | imageSuffix url = RawImg url
      | otherwise = Reject url


