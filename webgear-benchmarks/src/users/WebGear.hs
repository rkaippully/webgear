{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}

module WebGear where

import Control.Applicative (Alternative (..))
import Control.Arrow (Kleisli (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT, runReaderT)
import Data.ByteString.Lazy (ByteString)
import Data.Tagged (Tagged (..))
import Network.HTTP.Types (StdMethod (..))
import Network.Wai (Application)

import Model
import WebGear.Middlewares
import WebGear.Trait
import WebGear.Types


--------------------------------------------------------------------------------
-- Routes of the API
--------------------------------------------------------------------------------
type IntUserId = PathVar "userId" Int

userRoutes :: (forall req a. Handler' (ReaderT UserStore IO) req a -> Handler req a)
           -> Handler '[] ByteString
userRoutes toRouter = [match| /v1/users/userId:Int |]   -- non-TH version: path @"/v1/users" . pathVar @"userId" @Int
                      $ getUser <|> putUser <|> deleteUser
  where
    getUser :: Has IntUserId req => Handler req ByteString
    getUser = method @GET
              $ jsonResponseBody @User
              $ toRouter getUserHandler

    putUser :: Has IntUserId req => Handler req ByteString
    putUser = method @PUT
              $ requestContentTypeHeader @"application/json"
              $ jsonRequestBody @User
              $ jsonResponseBody @User
              $ toRouter putUserHandler

    deleteUser :: Has IntUserId req => Handler req ByteString
    deleteUser = method @DELETE
                 $ toRouter deleteUserHandler

getUserHandler :: ( MonadReader UserStore m
                  , MonadIO m
                  , Has IntUserId req
                  )
               => Handler' m req User
getUserHandler = Kleisli $ \request -> do
  let uid = get $ Tagged @IntUserId request
  store <- ask
  user <- lookupUser store (UserId uid)
  pure $ maybe notFound404 ok200 user

putUserHandler :: ( MonadReader UserStore m
                  , MonadIO m
                  , Have [IntUserId, JSONRequestBody User] req
                  )
               => Handler' m req User
putUserHandler = Kleisli $ \request -> do
  let uid   = get $ Tagged @IntUserId request
      user  = get $ Tagged @(JSONRequestBody User) request
      user' = user { userId = UserId uid }
  store <- ask
  addUser store user'
  pure $ ok200 user'

deleteUserHandler :: ( MonadReader UserStore m
                     , MonadIO m
                     , Has IntUserId req
                     )
                  => Handler' m req ByteString
deleteUserHandler = Kleisli $ \request -> do
  let uid = get $ Tagged @IntUserId request
  store <- ask
  found <- removeUser store (UserId uid)
  pure $ if found then noContent204 else notFound404


--------------------------------------------------------------------------------
-- | The application server
--------------------------------------------------------------------------------
application :: UserStore -> Application
application store req cont =  runRoute (userRoutes $ transform appToRouter) req >>= cont
  where
    appToRouter :: ReaderT UserStore IO a -> Router a
    appToRouter = liftIO . flip runReaderT store
