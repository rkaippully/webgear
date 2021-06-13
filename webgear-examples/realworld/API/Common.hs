{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
module API.Common where

import Control.Exception.Safe (MonadCatch, MonadThrow)
import Control.Lens (view)
import Control.Monad.Except (MonadError)
import Control.Monad.Time (MonadTime (..))
import qualified Crypto.JWT as JWT
import Data.Aeson
import Data.Pool (Pool)
import Database.Persist.Sql (runSqlPool, toSqlKey)
import Database.Persist.Sqlite (SqlBackend)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Model.Common (DBAction)
import Model.Entities (Key, User)
import Relude
import WebGear


-- The API handlers run in the App monad.

data AppEnv = AppEnv
  { appEnvSqlBackend :: Pool SqlBackend
  , appEnvJWK        :: JWT.JWK
  }

newtype App a = App { unApp :: ReaderT AppEnv Router a }
  deriving newtype ( Functor, Applicative, Alternative, Monad, MonadPlus, MonadThrow, MonadCatch
                   , MonadReader AppEnv, MonadError RouteError, MonadState PathInfo, MonadIO)

instance MonadRouter App where
  rejectRoute = App $ lift rejectRoute
  errorResponse = App . lift . errorResponse
  catchErrorResponse (App (ReaderT action)) handler = App $ ReaderT $ \r ->
    catchErrorResponse (action r) (flip runReaderT r . unApp . handler)

instance MonadTime App where
  currentTime = liftIO currentTime

instance JWT.MonadRandom App where
  getRandomBytes = liftIO . JWT.getRandomBytes

askConnectionPool :: MonadReader AppEnv m => m (Pool SqlBackend)
askConnectionPool = asks appEnvSqlBackend

askJWK :: MonadReader AppEnv m => m JWT.JWK
askJWK = asks appEnvJWK

runDBAction :: DBAction a -> App a
runDBAction action = do
  pool <- askConnectionPool
  liftIO $ runSqlPool action pool


--------------------------------------------------------------------------------

-- Middlewares for JWT authentication with "token" scheme

type RequiredAuth = JWTAuth' Required "token" App () (Key User)
type OptionalAuth = JWTAuth' Optional "token" App () (Key User)

requiredTokenAuth :: RequestMiddleware' App req (RequiredAuth : req) a
requiredTokenAuth = tokenAuth jwtAuth'

optionalTokenAuth :: RequestMiddleware' App req (OptionalAuth : req) a
optionalTokenAuth = tokenAuth optionalJWTAuth'

tokenAuth :: (JWTAuthConfig App () (Key User) -> RequestMiddleware' App req (r:req) a)
          -> RequestMiddleware' App req (r:req) a
tokenAuth auth handler = Kleisli $ \request -> do
  jwk <- askJWK
  let handler' = auth JWTAuthConfig{jwkSet = JWT.JWKSet [jwk], ..} handler
  runKleisli handler' request
  where
    jwtAuthRealm = "realworld"
    jwtValidationSettings = JWT.defaultJWTValidationSettings $ const True

    toJWTAttribute :: JWT.ClaimsSet -> App (Either () (Key User))
    toJWTAttribute claims = pure $
      case view JWT.claimSub claims >>= readMaybe . toString . view JWT.string of
        Nothing  -> Left ()
        Just oid -> Right $ toSqlKey oid


--------------------------------------------------------------------------------

-- A "wrapped" json body. Realworld API spec consumes and returns JSON
-- objects wrapped under a key in a top level object. The @Wrapped@
-- type encodes/decodes such objects.

newtype Wrapped (s :: Symbol) t = Wrapped { unwrap :: t }

instance (KnownSymbol s, FromJSON t) => FromJSON (Wrapped s t) where
  parseJSON = withObject "json object" $ \obj ->
    Wrapped <$> obj .: fromString (symbolVal $ Proxy @s)

instance (KnownSymbol s, ToJSON t) => ToJSON (Wrapped s t) where
  toJSON (Wrapped x) = object [fromString (symbolVal $ Proxy @s) .= toJSON x]
