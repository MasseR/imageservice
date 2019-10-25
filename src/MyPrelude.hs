module MyPrelude
  ( module X
  , tshow
  , unlessM
  , ByteString
  , LByteString
  , note
  )
  where

import           Control.Monad        as X (forM, forM_, forever, unless, void)
import           Control.Monad.Reader as X (MonadReader, ReaderT (..), ask,
                                            asks, runReaderT)
import           Data.Bool            as X (bool)
import           Data.Foldable        as X (foldl', for_)
import           Data.Int             as X (Int64)
import           Data.List            as X (intercalate, isInfixOf)
import           Data.Map.Strict      as X (Map)
import           Data.Maybe           as X (isJust)
import           Data.Set             as X (Set, member)
import           Data.Text            as X (Text, pack, unpack)
import           Data.Text.Encoding   as X (encodeUtf8)
import           Data.Text.IO         as X (putStrLn)
import           Data.Time            as X (Day, UTCTime (..),
                                            defaultTimeLocale, formatTime,
                                            getCurrentTime)
import           GHC.Generics         as X (Generic)
import           Prelude              as X hiding (lookup, putStrLn, readFile)
import           UnliftIO             as X hiding (Handler)

import           Data.ByteString      as X (readFile)
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL

type ByteString = B.ByteString
type LByteString = BL.ByteString

tshow :: Show a => a -> Text
tshow = pack . show

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM predicate act = predicate >>= \p -> unless p act

note :: e -> Maybe a -> Either e a
note e = maybe (Left e) Right
