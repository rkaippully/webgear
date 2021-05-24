{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Control.Exception.Safe (MonadCatch, MonadThrow)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadPlus, MonadReader, ReaderT (..), asks, lift)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Time (MonadTime (..))
import qualified Crypto.JWT as JWT
import Database.SQLite.Simple (Connection)
import WebGear


newtype AppM a = AppM { unAppM :: ReaderT (Connection, JWT.JWK) Router a }
  deriving newtype ( Functor, Applicative, Alternative, Monad, MonadPlus, MonadThrow, MonadCatch
                   , MonadReader (Connection, JWT.JWK), MonadError RouteError, MonadState PathInfo, MonadIO)

instance MonadRouter AppM where
  rejectRoute = AppM $ lift rejectRoute
  errorResponse = AppM . lift . errorResponse
  catchErrorResponse (AppM (ReaderT action)) handler = AppM $ ReaderT $ \r ->
    catchErrorResponse (action r) (flip runReaderT r . unAppM . handler)

instance MonadTime AppM where
  currentTime = liftIO currentTime

instance JWT.MonadRandom AppM where
  getRandomBytes = liftIO . JWT.getRandomBytes

askConnection :: AppM Connection
askConnection = asks fst

askJWK :: AppM JWT.JWK
askJWK = asks snd
