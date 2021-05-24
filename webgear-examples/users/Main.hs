{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeApplications      #-}

module Main where

import Control.Applicative (Alternative (..))
import Control.Arrow (Kleisli (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT, runReaderT)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Lazy (ByteString)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.Maybe (isJust)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Time.Calendar (Day)
import GHC.Generics (Generic)
import Network.HTTP.Types (StdMethod (..))
import Network.Wai (Application)
import qualified Network.Wai.Handler.Warp as Warp
import WebGear.Middlewares
import WebGear.Trait
import WebGear.Types


--------------------------------------------------------------------------------
-- An example program that uses WebGear to build a simple HTTP API to
-- perform CRUD operations on user records.
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Model for users
--------------------------------------------------------------------------------
data User = User
  { userId       :: UserId
  , userName     :: Text
  , dateOfBirth  :: Day
  , gender       :: Gender
  , emailAddress :: Text
  }
  deriving (Generic, FromJSON, ToJSON)

newtype UserId = UserId Int
  deriving (Eq, FromJSON, ToJSON, Hashable) via Int

data Gender = Male | Female | OtherGender
  deriving (Generic, FromJSON, ToJSON)


--------------------------------------------------------------------------------
-- An in-memory store and associated operations for users
--------------------------------------------------------------------------------
newtype UserStore = UserStore (IORef (HM.HashMap UserId User))

addUser :: MonadIO m => UserStore -> User -> m ()
addUser (UserStore ref) user = liftIO $ modifyIORef ref (HM.insert (userId user) user)

lookupUser :: MonadIO m => UserStore -> UserId -> m (Maybe User)
lookupUser (UserStore ref) uid = liftIO (HM.lookup uid <$> readIORef ref)

removeUser :: MonadIO m => UserStore -> UserId -> m Bool
removeUser store@(UserStore ref) uid = liftIO $ do
  u <- lookupUser store uid
  modifyIORef ref (HM.delete uid)
  pure $ isJust u


--------------------------------------------------------------------------------
-- Routes of the API
--------------------------------------------------------------------------------
type IntUserId = PathVar "userId" Int

type Auth = BasicAuth Router () Credentials

authConfig :: BasicAuthConfig Router () Credentials
authConfig = BasicAuthConfig
  { basicAuthRealm   = "Wakanda"
  , toBasicAttribute = \creds -> pure $
                         if creds == Credentials "panther" "forever"
                         then Right creds
                         else Left ()
  }

userRoutes :: (forall req a. Handler' (ReaderT UserStore IO) req a -> Handler req a)
           -> Handler '[] ByteString
userRoutes handler = allRoutes
  where
    allRoutes :: Handler '[] ByteString
    allRoutes = [match| /v1/users/userId:Int |]   -- non-TH version: path @"/v1/users" . pathVar @"userId" @Int
                (publicRoutes <|> protectedRoutes)

    -- | Routes accessible without any authentication
    publicRoutes :: Has IntUserId req => Handler req ByteString
    publicRoutes = getUser

    -- | Routes that require HTTP basic authentication
    protectedRoutes :: Has IntUserId req => Handler req ByteString
    protectedRoutes = basicAuth authConfig
                      $ putUser <|> deleteUser

    getUser :: Has IntUserId req => Handler req ByteString
    getUser = method @GET
              $ jsonResponseBody @User
              $ handler getUserHandler

    putUser :: Have [Auth, IntUserId] req => Handler req ByteString
    putUser = method @PUT
              $ requestContentTypeHeader @"application/json"
              $ jsonRequestBody @User
              $ jsonResponseBody @User
              $ handler putUserHandler

    deleteUser :: Have [Auth, IntUserId] req => Handler req ByteString
    deleteUser = method @DELETE
                 $ handler deleteUserHandler

getUserHandler :: ( MonadReader UserStore m
                  , MonadIO m
                  , Has IntUserId req
                  )
               => Handler' m req User
getUserHandler = Kleisli $ \request -> do
  let uid = get (Proxy @IntUserId) request
  store <- ask
  user <- lookupUser store (UserId uid)
  pure $ maybe notFound404 ok200 user

putUserHandler :: ( MonadReader UserStore m
                  , MonadIO m
                  , Have [Auth, IntUserId, JSONBody User]  req
                  )
               => Handler' m req User
putUserHandler = Kleisli $ \request -> do
  let uid  = get (Proxy @IntUserId) request
      user = get (Proxy @(JSONBody User)) request
      user'       = user { userId = UserId uid }
  store <- ask
  addUser store user'
  logActivity request "updated"
  pure $ ok200 user'

deleteUserHandler :: ( MonadReader UserStore m
                     , MonadIO m
                     , Have [Auth, IntUserId] req
                     )
                  => Handler' m req ByteString
deleteUserHandler = Kleisli $ \request -> do
  let uid = get (Proxy @IntUserId) request
  store <- ask
  found <- removeUser store (UserId uid)
  if found
    then logActivity request "deleted" >> pure noContent204
    else pure notFound404

logActivity :: (MonadIO m, Has Auth req) => Linked req Request -> String -> m ()
logActivity request msg = do
  let name = credentialsUsername $ get (Proxy @Auth) request
  liftIO $ putStrLn $ msg <> ": by " <> show name


--------------------------------------------------------------------------------
-- | The application server
--------------------------------------------------------------------------------
application :: UserStore -> Application
application store req cont = runRoute (userRoutes $ transform appToRouter) req >>= cont
  where
    appToRouter :: ReaderT UserStore IO a -> Router a
    appToRouter = liftIO . flip runReaderT store

main :: IO ()
main = do
  store <- newIORef HM.empty
  Warp.run 3000 (application $ UserStore store)
