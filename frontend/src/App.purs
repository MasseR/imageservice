module App
  ( AppM
  , Env(..)
  , runAppM
  ) where

import Prelude

import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, runReaderT)

import Effect.Class (class MonadEffect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)

data Env
  = Env { host :: String }

newtype AppM a = AppM (ReaderT Env Aff a)

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM
-- Can write newtype instance for proper data but not synonyms
derive newtype instance monadAskAppM :: MonadAsk Env AppM

-- instance affApp :: MonadAff AppM where
--   liftAff = ?lift

runAppM :: Env -> AppM ~> Aff
runAppM env (AppM f) = runReaderT f env
