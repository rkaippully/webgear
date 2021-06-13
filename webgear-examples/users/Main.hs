{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeApplications           #-}
module Main where

import Control.Applicative (Alternative (..))
import Control.Arrow (Kleisli (..))
import Control.Monad (MonadPlus)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.State.Strict (MonadState)
import Control.Monad.Trans (lift)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Lazy (ByteString)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.Maybe (isJust)
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
type Auth = BasicAuth App () Credentials

authConfig :: BasicAuthConfig App () Credentials
authConfig = BasicAuthConfig
  { basicAuthRealm   = "Wakanda"
  , toBasicAttribute = \creds -> pure $
                         if creds == Credentials "panther" "forever"
                         then Right creds
                         else Left ()
  }

-- The route handlers run in the App monad
newtype App a = App { unApp :: ReaderT UserStore Router a }
  deriving newtype ( Functor, Applicative, Alternative, Monad, MonadPlus
                   , MonadIO , MonadReader UserStore, MonadError RouteError, MonadState PathInfo)

instance MonadRouter App where
  rejectRoute = App $ lift rejectRoute
  errorResponse = App . lift . errorResponse
  catchErrorResponse (App (ReaderT action)) handler = App $ ReaderT $ \r ->
    catchErrorResponse (action r) (flip runReaderT r . unApp . handler)

userRoutes :: Handler' App '[] ByteString
userRoutes = [match| /v1/users/userId:Int |]   -- non-TH version: path @"/v1/users" . pathVar @"userId" @Int
               (publicRoutes <|> protectedRoutes)

-- | Routes accessible without any authentication
publicRoutes :: HasTrait IntUserId req => Handler' App req ByteString
publicRoutes = getUser

-- | Routes that require HTTP basic authentication
protectedRoutes :: HasTrait IntUserId req => Handler' App req ByteString
protectedRoutes = basicAuth authConfig
                  $ putUser <|> deleteUser

getUser :: HasTrait IntUserId req => Handler' App req ByteString
getUser = method @GET
          $ jsonResponseBody @User
          $ getUserHandler

putUser :: HaveTraits [Auth, IntUserId] req => Handler' App req ByteString
putUser = method @PUT
          $ requestContentTypeHeader @"application/json"
          $ jsonRequestBody @User
          $ jsonResponseBody @User
          $ putUserHandler

deleteUser :: HaveTraits [Auth, IntUserId] req => Handler' App req ByteString
deleteUser = method @DELETE deleteUserHandler

getUserHandler :: ( MonadReader UserStore m
                  , MonadIO m
                  , HasTrait IntUserId req
                  )
               => Handler' m req User
getUserHandler = Kleisli $ \request -> do
  let uid = pick @IntUserId $ from request
  store <- ask
  user <- lookupUser store (UserId uid)
  pure $ maybe notFound404 ok200 user

putUserHandler :: ( MonadReader UserStore m
                  , MonadIO m
                  , HaveTraits [Auth, IntUserId, JSONBody User]  req
                  )
               => Handler' m req User
putUserHandler = Kleisli $ \request -> do
  let uid  = pick @IntUserId $ from request
      user = pick @(JSONBody User) $ from request
      user'       = user { userId = UserId uid }
  store <- ask
  addUser store user'
  logActivity request "updated"
  pure $ ok200 user'

deleteUserHandler :: ( MonadReader UserStore m
                     , MonadIO m
                     , HaveTraits [Auth, IntUserId] req
                     )
                  => Handler' m req ByteString
deleteUserHandler = Kleisli $ \request -> do
  let uid = pick @IntUserId $ from request
  store <- ask
  found <- removeUser store (UserId uid)
  if found
    then logActivity request "deleted" >> pure noContent204
    else pure notFound404

logActivity :: (MonadIO m, HasTrait Auth req) => Linked req Request -> String -> m ()
logActivity request msg = do
  let name = credentialsUsername $ pick @Auth $ from request
  liftIO $ putStrLn $ msg <> ": by " <> show name


--------------------------------------------------------------------------------
-- | The application server
--------------------------------------------------------------------------------
application :: UserStore -> Application
application store = toApplication $ transform appToRouter userRoutes
  where
    appToRouter :: App a -> Router a
    appToRouter = flip runReaderT store . unApp

main :: IO ()
main = do
  store <- newIORef HM.empty
  Warp.run 3000 (application $ UserStore store)
