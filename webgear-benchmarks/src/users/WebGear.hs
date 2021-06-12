{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeApplications           #-}

module WebGear where

import Control.Applicative (Alternative (..))
import Control.Arrow (Kleisli (..))
import Control.Monad (MonadPlus)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.State.Strict (MonadState)
import Control.Monad.Trans (lift)
import Data.ByteString.Lazy (ByteString)
import Model
import Network.HTTP.Types (StdMethod (..))
import Network.Wai (Application)
import WebGear.Middlewares
import WebGear.Trait
import WebGear.Types


--------------------------------------------------------------------------------
-- Routes of the API
--------------------------------------------------------------------------------
type IntUserId = PathVar "userId" Int

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
             $ getUser <|> putUser <|> deleteUser

getUser :: HasTrait IntUserId req => Handler' App req ByteString
getUser = method @GET
          $ jsonResponseBody @User
          $ getUserHandler

putUser :: HasTrait IntUserId req => Handler' App req ByteString
putUser = method @PUT
          $ requestContentTypeHeader @"application/json"
          $ jsonRequestBody @User
          $ jsonResponseBody @User
          $ putUserHandler

deleteUser :: HasTrait IntUserId req => Handler' App req ByteString
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
                  , HaveTraits [IntUserId, JSONBody User] req
                  )
               => Handler' m req User
putUserHandler = Kleisli $ \request -> do
  let uid   = pick @IntUserId $ from request
      user  = pick @(JSONBody User) $ from request
      user' = user { userId = UserId uid }
  store <- ask
  addUser store user'
  pure $ ok200 user'

deleteUserHandler :: ( MonadReader UserStore m
                     , MonadIO m
                     , HasTrait IntUserId req
                     )
                  => Handler' m req ByteString
deleteUserHandler = Kleisli $ \request -> do
  let uid = pick @IntUserId $ from request
  store <- ask
  found <- removeUser store (UserId uid)
  pure $ if found then noContent204 else notFound404


--------------------------------------------------------------------------------
-- | The application server
--------------------------------------------------------------------------------
application :: UserStore -> Application
application store = toApplication $ transform appToRouter userRoutes
  where
    appToRouter :: App a -> Router a
    appToRouter = flip runReaderT store . unApp
