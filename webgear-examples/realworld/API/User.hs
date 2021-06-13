{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module API.User
  ( create
  , login
  , current
  , update
  ) where

import API.Common
import Control.Exception.Safe (catch)
import qualified Database.Sqlite as DB
import qualified Model.User as Model
import Relude
import WebGear


type CreateUserRequest = Wrapped "user" Model.CreateUserPayload
type UserResponse = Wrapped "user" Model.UserRecord

create :: Handler' App req LByteString
create = jsonRequestBody @CreateUserRequest
         $ jsonResponseBody @UserResponse
         $ handler
  where
    handler = Kleisli $ \request -> do
      let userPayload = pick @(JSONBody CreateUserRequest) $ from request
      jwk <- askJWK
      let doCreate = do
            user <- runDBAction $ Model.create jwk (unwrap userPayload)
            pure $ ok200 $ Wrapped user
      doCreate `catch` handleDBError

handleDBError :: DB.SqliteException -> App (Response a)
handleDBError e | DB.seError e == DB.ErrorConstraint = errorResponse $ badRequest400 "Another user account exists with these values"
                | otherwise = errorResponse $ internalServerError500 $ fromString $ show e



--------------------------------------------------------------------------------

type LoginUserRequest = Wrapped "user" Model.LoginUserPayload

login :: Handler' App req LByteString
login = jsonRequestBody @LoginUserRequest
        $ jsonResponseBody @UserResponse
        $ handler
  where
    handler = Kleisli $ \request -> do
      let loginPayload = pick @(JSONBody LoginUserRequest) $ from request
      jwk <- askJWK
      maybeUser <- runDBAction $ Model.checkCredentials jwk (unwrap loginPayload)
      maybe forbidden (pure . ok200 . Wrapped) maybeUser

    forbidden = errorResponse $ forbidden403 "Invalid credentials"


--------------------------------------------------------------------------------

current :: Handler' App req LByteString
current = requiredTokenAuth
          $ jsonResponseBody @UserResponse
          $ handler
  where
    handler :: HasTrait RequiredAuth req => Handler' App req UserResponse
    handler = Kleisli $ \request -> do
      let userId = pick @RequiredAuth $ from request
      jwk <- askJWK
      maybeUser <- runDBAction $ Model.getByKey jwk userId
      pure $ maybe notFound404 (ok200 . Wrapped) maybeUser


--------------------------------------------------------------------------------

type UpdateUserRequest = Wrapped "user" Model.UpdateUserPayload

update :: Handler' App req LByteString
update = requiredTokenAuth
         $ jsonRequestBody @UpdateUserRequest
         $ jsonResponseBody @UserResponse
         $ handler
  where
    handler :: HaveTraits [RequiredAuth, JSONBody UpdateUserRequest] req => Handler' App req UserResponse
    handler = Kleisli $ \request -> do
      let userId = pick @RequiredAuth $ from request
          userPayload = pick @(JSONBody UpdateUserRequest) $ from request
      jwk <- askJWK
      let doUpdate = do
            maybeUser <- runDBAction $ Model.update jwk userId (unwrap userPayload)
            pure $ maybe notFound404 (ok200 . Wrapped) maybeUser
      doUpdate `catch` handleDBError
