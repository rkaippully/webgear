{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import Control.Applicative (Alternative (..))
import Control.Arrow (Kleisli (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), runReaderT)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Lazy (ByteString)
import Data.Hashable (Hashable)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.Maybe (isJust)
import Data.Tagged (Tagged (..))
import Data.Text (Text)
import Data.Time.Calendar (Day)
import GHC.Generics (Generic)
import Network.HTTP.Types (StdMethod (..))
import Network.Wai (Application)

import WebGear.Middlewares
import WebGear.Route
import WebGear.Trait
import WebGear.Types

import qualified Data.HashMap.Strict as HM
import qualified Network.Wai.Handler.Warp as Warp


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

userRoutes :: ( MonadRouter m
              , MonadReader UserStore m
              , MonadIO m
              )
           => Handler m '[] '[] ByteString
userRoutes = [match| v1/users/userId:Int |]   -- non-TH version: path @"v1/users" . pathVar @"userId" @Int
             (publicRoutes <|> protectedRoutes)

-- | Routes accessible without any authentication
publicRoutes :: ( MonadRouter m
                , Has IntUserId req
                , MonadReader UserStore m
                , MonadIO m
                )
             => Handler m req '[] ByteString
publicRoutes = getUser

-- | Routes that require HTTP basic authentication
protectedRoutes :: ( MonadRouter m
                   , Has IntUserId req
                   , MonadReader UserStore m
                   , MonadIO m
                   )
                => Handler m req '[] ByteString
protectedRoutes = basicAuth "Wakanda" isValidCreds
                  $ putUser <|> deleteUser

isValidCreds :: Monad m => Credentials -> m Bool
isValidCreds creds = pure $
  credentialsUsername creds == "panther"
  && credentialsPassword creds == "forever"

getUser :: ( MonadRouter m
           , Has IntUserId req
           , MonadReader UserStore m
           , MonadIO m
           )
        => Handler m req '[] ByteString
getUser = method @GET
          $ jsonResponseBody @User
          $ getUserHandler

putUser :: ( MonadRouter m
           , Has IntUserId req
           , Has BasicAuth req
           , MonadReader UserStore m
           , MonadIO m
           )
        => Handler m req '[] ByteString
putUser = method @PUT
          $ requestContentType @"application/json"
          $ jsonRequestBody @User
          $ jsonResponseBody @User
          $ putUserHandler

deleteUser :: ( MonadRouter m
              , Has IntUserId req
              , Has BasicAuth req
              , MonadReader UserStore m
              , MonadIO m
              )
           => Handler m req '[] ByteString
deleteUser = method @DELETE
             $ deleteUserHandler

getUserHandler :: ( MonadReader UserStore m
                  , MonadIO m
                  , Has IntUserId req
                  )
               => Handler m req '[] User
getUserHandler = Kleisli $ \request -> do
  let Tagged uid = get @IntUserId request
  store <- ask
  user <- lookupUser store (UserId uid)
  maybe notFound404 ok200 user

logActivity :: (MonadIO m, Has BasicAuth req) => Linked req Request -> String -> m ()
logActivity request msg = do
  let Tagged name = credentialsUsername <$> get @BasicAuth request
  liftIO $ putStrLn $ msg <> ": by " <> show name

putUserHandler :: ( MonadReader UserStore m
                  , MonadIO m
                  , Has IntUserId req
                  , Has (JSONRequestBody User) req
                  , Has BasicAuth req
                  )
               => Handler m req '[] User
putUserHandler = Kleisli $ \request -> do
  let Tagged uid  = get @IntUserId request
      Tagged user = get @(JSONRequestBody User) request
      user'       = user { userId = UserId uid }
  store <- ask
  addUser store user'
  logActivity request "updated"
  ok200 user'

deleteUserHandler :: ( MonadReader UserStore m
                     , MonadIO m
                     , Has IntUserId req
                     , Has BasicAuth req
                     )
                  => Handler m req '[] ByteString
deleteUserHandler = Kleisli $ \request -> do
  let Tagged uid = get @IntUserId request
  store <- ask
  found <- removeUser store (UserId uid)
  if found
    then logActivity request "deleted" >> noContent204
    else notFound404


--------------------------------------------------------------------------------
-- | The application server
--------------------------------------------------------------------------------
application :: UserStore -> Application
application store req cont = runReaderT (runRoute userRoutes req) store >>= cont

main :: IO ()
main = do
  store <- newIORef HM.empty
  Warp.run 3000 (application $ UserStore store)
