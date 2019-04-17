{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
module Worker.Indexer.Reddit where

import           ClassyPrelude
import           Control.Lens
import           Control.Monad.Catch   (MonadThrow)
import           Data.Aeson.Lens
import           Data.Generics.Product
import           Data.List.Split       (chunksOf)
import           Logging
import           Network.HTTP.Client

newtype Subreddit = Subreddit { getSubreddit :: String }
newtype ImgHref = ImgHref { getImg :: String }

-- Consider going to full types, they're just a bit complex given the time I have

images :: (HasLog m, HasType Manager r, MonadThrow m, MonadReader r m, MonadUnliftIO m) => [Subreddit] -> m [ImgHref]
images rs = concat <$> mapM multireddit (mkMultireddit rs)
  where
    multireddit r = do
      logLevel Info $ "Fetching images for " <> tshow r
      manager <- view (typed @Manager)
      urls <- imageUrls . getUrls <$> getLbs manager ("https://www.reddit.com/r/" <> r <> "/new.json")
      logLevel Debug $ "Rejecting: " <> (tshow (lefts urls))
      return (rights urls)
    imageUrls :: [Text] -> [Either Text ImgHref]
    imageUrls = map (\x -> bool (Left x) (Right . ImgHref . unpack $ x) (imageSuffix x))
    imageSuffix :: Text -> Bool
    imageSuffix x = suffix x `member` (["jpg", "png"] :: Set Text)
    suffix = reverse . takeWhile (/= '.') . reverse
    getUrls json = json ^.. key "data" . key "children" . _Array . traverse . key "data" . key "url" . _String
    mkMultireddit :: [Subreddit] -> [String]
    mkMultireddit = map (intercalate "+") . chunksOf 5 . map getSubreddit

