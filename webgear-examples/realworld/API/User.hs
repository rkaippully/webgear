{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module API.User
  ( create
  , login
  , current
  , update
  ) where

import API.Util
import Control.Monad.IO.Class (liftIO)
import qualified Crypto.JWT as JWT
import Data.Aeson (FromJSON, Object, ToJSON, (.=))
import Data.ByteString.Lazy (toStrict)
import Data.Functor ((<&>))
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics (Generic)
import Model
import Types
import WebGear


newtype UserWrapper u = UserWrapper
  { user :: u }
  deriving (Generic, FromJSON, ToJSON)

data UpdateUserInput = UpdateUserInput
  { username :: Maybe Text
  , email    :: Maybe Text
  , password :: Maybe Text
  , bio      :: Maybe Text
  , image    :: Maybe Text
  }
  deriving (Generic, FromJSON)

data UserOutput = UserOutput
  { username :: Text
  , email    :: Text
  , bio      :: Maybe Text
  , image    :: Maybe Text
  , token    :: Text
  }
  deriving (Generic, ToJSON)

data CreateUserInput = CreateUserInput
  { username :: Text
  , email    :: Text
  , password :: Text
  }
  deriving (Generic, FromJSON)

type CreateUserRequest = UserWrapper CreateUserInput
type UpdateUserRequest = UserWrapper UpdateUserInput
type UserResponse = UserWrapper UserOutput

create :: Handler' AppM req ByteString
create =
  jsonRequestBody @CreateUserRequest
  $ jsonResponseBody @UserResponse
  $ handler
  where
    handler = Kleisli $ \request -> do
      let CreateUserInput{..} = user $ get (Proxy @(JSONBody CreateUserRequest)) request
      createUser User{key = PrimaryKey 0, bio = Nothing, image = Nothing, ..} >>= \case
        Left _ -> errorResponse $ badRequest400 "User already exists"
        Right key -> do
          jwt <- generateJWT key
          pure $ ok200 $ UserWrapper $ UserOutput
            { bio = Nothing, image = Nothing, token = jwt, .. }


data LoginUserInput = LoginUserInput
  { email    :: Text
  , password :: Text
  }
  deriving (Generic, FromJSON)

type LoginUserRequest = UserWrapper LoginUserInput

login :: Handler' AppM req ByteString
login =
  jsonRequestBody @LoginUserRequest
  $ jsonResponseBody @UserResponse
  $ handler
  where
    handler = Kleisli $ \request -> do
      let LoginUserInput{..} = user $ get (Proxy @(JSONBody LoginUserRequest)) request
      getUserByEmail email >>= maybe forbidden (checkPassword password)

    forbidden = errorResponse $ forbidden403 "Invalid credentials"

    checkPassword plainPwd User{..} =
      if hashUserPassword plainPwd /= password
        then forbidden
        else do
          jwt <- generateJWT key
          pure $ ok200 $ UserWrapper $ UserOutput{token = jwt, ..}


current :: Handler' AppM req ByteString
current = requiredJWTAuth
          $ jsonResponseBody @UserResponse
          $ handler
  where
    handler :: Has RequiredAuth req => Handler' AppM req UserResponse
    handler = Kleisli $ \request -> do
      let jwtKey = get (Proxy @RequiredAuth) request
      getUserById jwtKey >>= \case
        Nothing -> pure notFound404
        Just User{..} -> do
          jwt <- generateJWT key
          pure $ ok200 $ UserWrapper $ UserOutput {token=jwt, ..}


update :: Handler' AppM req ByteString
update =
  requiredJWTAuth
  $ jsonRequestBody @UpdateUserRequest
  $ jsonResponseBody @UserResponse
  $ handler
  where
    handler :: Have [RequiredAuth, JSONBody UpdateUserRequest] req
            => Handler' AppM req UserResponse
    handler = Kleisli $ \request -> do
      let UpdateUserInput{..} = user $ get (Proxy @(JSONBody UpdateUserRequest)) request
      let key = get (Proxy @RequiredAuth) request
      updateUser User{bio = Just bio, image = Just bio, ..} >>= mkResponse

    mkResponse = \case
      Left _  -> errorResponse $ badRequest400 "Conflicting user attributes"
      Right Nothing -> pure notFound404
      Right (Just User{..}) -> do
        token <- generateJWT key
        pure $ ok200 $ UserWrapper $ UserOutput{..}


-- JWT util functions

generateJWT :: PrimaryKey -> AppM Text
generateJWT key = do
  let mkError :: JWT.JWTError -> AppM a
      mkError e = do
        liftIO $ print e
        errorResponse $ internalServerError500 "internal error"
  result <- claimsToJWT ["sub" .= show key]
  either mkError pure result

claimsToJWT :: Object -> AppM (Either JWT.JWTError Text)
claimsToJWT claims = do
  jwk <- askJWK
  mkJWT jwk claims <&> fmap (decodeUtf8 . toStrict . JWT.encodeCompact)
