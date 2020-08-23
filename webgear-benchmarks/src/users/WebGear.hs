{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications  #-}

module WebGear where

import Control.Applicative (Alternative (..))
import Control.Arrow (Kleisli (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), runReaderT)
import Data.ByteString.Lazy (ByteString)
import Data.Tagged (Tagged (..))
import Network.HTTP.Types (StdMethod (..))
import Network.Wai (Application)

import Model
import WebGear.Middlewares
import WebGear.Route
import WebGear.Trait
import WebGear.Types


--------------------------------------------------------------------------------
-- Routes of the API
--------------------------------------------------------------------------------
type IntUserId = PathVar "userId" Int

userRoutes :: ( MonadRouter m
              , MonadReader UserStore m
              , MonadIO m
              )
           => Handler m '[] ByteString
userRoutes = [match| v1/users/userId:Int |]   -- non-TH version: path @"v1/users" . pathVar @"userId" @Int
             $ getUser <|> putUser <|> deleteUser

getUser :: ( MonadRouter m
           , Has IntUserId req
           , MonadReader UserStore m
           , MonadIO m
           )
        => Handler m req ByteString
getUser = method @GET
          $ jsonResponseBody @User
          $ getUserHandler

putUser :: ( MonadRouter m
           , Has IntUserId req
           , MonadReader UserStore m
           , MonadIO m
           )
        => Handler m req ByteString
putUser = method @PUT
          $ requestContentType @"application/json"
          $ jsonRequestBody @User
          $ jsonResponseBody @User
          $ putUserHandler

deleteUser :: ( MonadRouter m
              , Has IntUserId req
              , MonadReader UserStore m
              , MonadIO m
              )
           => Handler m req ByteString
deleteUser = method @DELETE
             $ deleteUserHandler

getUserHandler :: ( MonadReader UserStore m
                  , MonadIO m
                  , Has IntUserId req
                  )
               => Handler m req User
getUserHandler = Kleisli $ \request -> do
  let Tagged uid = get @IntUserId request
  store <- ask
  user <- lookupUser store (UserId uid)
  maybe notFound404 ok200 user

putUserHandler :: ( MonadReader UserStore m
                  , MonadIO m
                  , Has IntUserId req
                  , Has (JSONRequestBody User) req
                  )
               => Handler m req User
putUserHandler = Kleisli $ \request -> do
  let Tagged uid  = get @IntUserId request
      Tagged user = get @(JSONRequestBody User) request
      user'       = user { userId = UserId uid }
  store <- ask
  addUser store user'
  ok200 user'

deleteUserHandler :: ( MonadReader UserStore m
                     , MonadIO m
                     , Has IntUserId req
                     )
                  => Handler m req ByteString
deleteUserHandler = Kleisli $ \request -> do
  let Tagged uid = get @IntUserId request
  store <- ask
  found <- removeUser store (UserId uid)
  if found then noContent204 else notFound404


--------------------------------------------------------------------------------
-- | The application server
--------------------------------------------------------------------------------
application :: UserStore -> Application
application store req cont = runReaderT (runRoute userRoutes req) store >>= cont
