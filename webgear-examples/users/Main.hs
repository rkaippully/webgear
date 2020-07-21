{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE DerivingVia      #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE TypeApplications #-}

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
import Data.Tagged (untag)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import GHC.Generics (Generic)
import Network.HTTP.Types (StdMethod (..))
import Network.Wai (Application)

import WebGear.Middlewares (jsonRequestBody, jsonResponseBody, match, method, noContent, notFound,
                            ok, requestContentType)
import WebGear.Route (MonadRouter, runRoute)
import WebGear.Trait (Has (..))
import WebGear.Trait.Body (JSONRequestBody)
import WebGear.Trait.Path (PathVar)
import WebGear.Types (Handler)

import qualified Data.HashMap.Strict as HM
import qualified Network.Wai.Handler.Warp as Warp


{-
  An example program that uses webgear to build a simple HTTP API to
  perform CRUD operations on users.
-}

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
-- An in-memory store for users. In a real program this would be stored in some
-- sort of a database.
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

userRoutes :: (MonadRouter m, MonadReader UserStore m, MonadIO m) => Handler m '[] '[] ByteString
userRoutes = [match| v1/users/userId:Int |] -- non-TH version: path @"v1/users" . pathVar @"userId" @Int
             $ getUser <|> putUser <|> deleteUser

getUser :: (MonadRouter m, Has IntUserId req, MonadReader UserStore m, MonadIO m)
        => Handler m req '[] ByteString
getUser = method @GET
          $ jsonResponseBody @User
          $ getUserHandler

putUser :: (MonadRouter m, Has IntUserId req, MonadReader UserStore m, MonadIO m)
        => Handler m req '[] ByteString
putUser = method @PUT
          $ requestContentType @"application/json"
          $ jsonRequestBody @User
          $ jsonResponseBody @User
          $ putUserHandler

deleteUser :: (MonadRouter m, Has IntUserId req, MonadReader UserStore m, MonadIO m)
           => Handler m req '[] ByteString
deleteUser = method @DELETE
             $ deleteUserHandler

getUserHandler :: (MonadReader UserStore m, MonadIO m, Has IntUserId req)
               => Handler m req '[] User
getUserHandler = Kleisli $ \request -> do
  let uid = untag $ traitValue @IntUserId request
  store <- ask
  user <- lookupUser store (UserId uid)
  maybe notFound ok user

putUserHandler :: (MonadReader UserStore m, MonadIO m, Has IntUserId req, Has (JSONRequestBody User) req)
               => Handler m req '[] User
putUserHandler = Kleisli $ \request -> do
  let uid   = untag $ traitValue @IntUserId request
      user  = untag $ traitValue @(JSONRequestBody User) request
      user' = user { userId = UserId uid }
  store <- ask
  addUser store user'
  ok user'

deleteUserHandler :: (MonadReader UserStore m, MonadIO m, Has IntUserId req)
                  => Handler m req '[] ByteString
deleteUserHandler = Kleisli $ \request -> do
  let uid = untag $ traitValue @IntUserId request
  store <- ask
  found <- removeUser store (UserId uid)
  if found then noContent else notFound


--------------------------------------------------------------------------------
-- | The application server
--------------------------------------------------------------------------------
application :: UserStore -> Application
application store req respond = runReaderT (runRoute userRoutes req) store >>= respond

main :: IO ()
main = do
  store <- newIORef HM.empty
  Warp.run 3000 (application $ UserStore store)
