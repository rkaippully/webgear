{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE DerivingVia      #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Applicative
import Control.Arrow
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson (FromJSON, ToJSON)
import Data.IORef
import Data.Map (Map)
import Data.Maybe (isJust)
import Data.Tagged (untag)
import Data.Text (Text)
import GHC.Generics

import WebGear.Middleware
import WebGear.Route
import WebGear.Trait
import WebGear.Trait.Body
import WebGear.Trait.Path
import WebGear.Types

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified Data.Time.Calendar as Time
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp


--------------------------------------------------------------------------------
-- | Model for users
--------------------------------------------------------------------------------
data User = User
  { userId       :: UserId
  , userName     :: Text
  , dateOfBirth  :: Time.Day
  , gender       :: Gender
  , emailAddress :: Text
  }
  deriving (Generic, FromJSON, ToJSON)

newtype UserId = UserId Int
  deriving (Eq, Ord)
  deriving (FromJSON, ToJSON) via Int

data Gender = Male | Female | OtherGender
  deriving (Generic, FromJSON, ToJSON)


--------------------------------------------------------------------------------
-- | An in-memory store for users
--------------------------------------------------------------------------------
type UserStore = IORef (Map UserId User)

addUser :: MonadIO m => UserStore -> User -> m ()
addUser store user = liftIO $ modifyIORef store (Map.insert (userId user) user)

lookupUser :: MonadIO m => UserStore -> UserId -> m (Maybe User)
lookupUser store uid = liftIO (Map.lookup uid <$> readIORef store)

removeUser :: MonadIO m => UserStore -> UserId -> m Bool
removeUser store uid = liftIO $ do
  u <- lookupUser store uid
  modifyIORef store (Map.delete uid)
  pure $ isJust u


--------------------------------------------------------------------------------
-- | Routes of the API
--------------------------------------------------------------------------------
type IntUserId = PathVar "userId" Int

userRoutes :: (MonadRouter m, MonadReader UserStore m, MonadIO m) => Handler m '[] '[] LBS.ByteString
userRoutes = [match| v1/users/userId:Int |] -- non-TH version: path @"v1/users" . pathVar @"userId" @Int
             $ getUser <|> putUser <|> deleteUser

getUser :: (MonadRouter m, Has IntUserId req, MonadReader UserStore m, MonadIO m)
        => Handler m req '[] LBS.ByteString
getUser = method @HTTP.GET
          $ jsonResponseBody @User
          $ getUserHandler

putUser :: (MonadRouter m, Has IntUserId req, MonadReader UserStore m, MonadIO m)
        => Handler m req '[] LBS.ByteString
putUser = method @HTTP.PUT
          $ requestContentType @"application/json"
          $ jsonRequestBody @User
          $ jsonResponseBody @User
          $ putUserHandler

deleteUser :: (MonadRouter m, Has IntUserId req, MonadReader UserStore m, MonadIO m)
           => Handler m req '[] LBS.ByteString
deleteUser = method @HTTP.DELETE
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
                  => Handler m req '[] LBS.ByteString
deleteUserHandler = Kleisli $ \request -> do
  let uid = untag $ traitValue @IntUserId request
  store <- ask
  found <- removeUser store (UserId uid)
  if found then noContent else notFound


--------------------------------------------------------------------------------
-- | The application server
--------------------------------------------------------------------------------
application :: UserStore -> Wai.Application
application store req respond = respond =<< runReaderT (unRoute userRoutes req) store

main :: IO ()
main = do
  store <- newIORef Map.empty
  Warp.run 3000 (application store)
