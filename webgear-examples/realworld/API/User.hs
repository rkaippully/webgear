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
import qualified Crypto.JWT as JWT
import Data.Aeson (FromJSON, Object, ToJSON, (.=))
import Data.ByteString.Lazy (toStrict)
import Data.Functor ((<&>))
import Data.String (fromString)
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
  , bio      :: Maybe (Maybe Text)
  , image    :: Maybe (Maybe Text)
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
      createUser User{userid = PrimaryKey Nothing, bio = Nothing, image = Nothing, ..} >>= \case
        Nothing -> errorResponse $ badRequest400 "User already exists"
        Just key -> do
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
      case unPrimaryKey userid of
        Nothing -> forbidden
        Just uid ->
          if hashUserPassword plainPwd /= password
          then forbidden
          else do
            jwt <- generateJWT uid
            pure $ ok200 $ UserWrapper $ UserOutput{token = jwt, ..}


current :: Handler' AppM req ByteString
current = requiredJWTAuth
          $ jsonResponseBody @UserResponse
          $ handler
  where
    handler :: Has RequiredAuth req => Handler' AppM req UserResponse
    handler = Kleisli $ \request -> do
      let jwtKey = get (Proxy @RequiredAuth) request
      getUserById (PrimaryKey $ Just jwtKey) >>= \case
        Nothing -> pure notFound404
        Just User{..} -> do
          jwt <- generateJWT jwtKey
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
      let uid = get (Proxy @RequiredAuth) request
          userM =
            let UpdateUserInput{..} = user $ get (Proxy @(JSONBody UpdateUserRequest)) request
            in UserM { useridM = uid
                     , usernameM = username
                     , emailM = email
                     , passwordM = password
                     , bioM = bio
                     , imageM = image
                     }

          mkResponse = \case
            --Left _  -> errorResponse $ badRequest400 "Conflicting user attributes"
            Nothing -> pure notFound404
            Just User{..} -> do
              token <- generateJWT uid
              pure $ ok200 $ UserWrapper $ UserOutput{..}

      updateUser userM
      getUserById (PrimaryKey $ Just uid) >>= mkResponse


-- JWT util functions

generateJWT :: Int -> AppM Text
generateJWT uid = do
  let mkError :: Show t => t -> AppM a
      mkError t = errorResponse $ internalServerError500 $ fromString $ "internal error: " <> show t
  result <- claimsToJWT ["sub" .= show uid]
  either mkError pure result

claimsToJWT :: Object -> AppM (Either JWT.JWTError Text)
claimsToJWT claims = do
  jwk <- askJWK
  mkJWT jwk claims <&> fmap (decodeUtf8 . toStrict . JWT.encodeCompact)
