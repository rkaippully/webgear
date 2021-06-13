{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeApplications      #-}
module Model.User
  ( CreateUserPayload (..)
  , UserRecord (..)
  , create
  , LoginUserPayload (..)
  , checkCredentials
  , getByKey
  , UpdateUserPayload (..)
  , Model.User.update
  ) where

import qualified Crypto.Hash as Hash
import qualified Crypto.JWT as JWT
import Data.Aeson
import Database.Esqueleto as E
import qualified Database.Persist.Sql as DB
import Model.Common
import Model.Entities
import Relude
import WebGear (mkJWT)


data CreateUserPayload = CreateUserPayload
  { userUsername :: Text
  , userEmail    :: Text
  , userPassword :: Text
  }
  deriving (Generic)

data UserRecord = UserRecord
  { userId       :: Int64
  , userUsername :: Text
  , userEmail    :: Text
  , userBio      :: Maybe Text
  , userImage    :: Maybe Text
  , userToken    :: Text
  }
  deriving (Generic)

instance FromJSON CreateUserPayload where
  parseJSON = genericParseJSON dropPrefixOptions

instance ToJSON UserRecord where
  toJSON = genericToJSON dropPrefixOptions

create :: JWT.JWK -> CreateUserPayload -> DBAction UserRecord
create jwk CreateUserPayload{..} = do
  let userBio = Nothing
      userImage = Nothing
  key <- DB.insert User{userPassword = hashUserPassword userPassword, ..}
  userToken <- generateJWT jwk key
  pure UserRecord{userId = DB.fromSqlKey key, ..}

hashUserPassword :: Text -> Text
hashUserPassword = show . Hash.hashWith Hash.SHA256 . (encodeUtf8 :: Text -> ByteString)

generateJWT :: JWT.JWK -> Key User -> DBAction Text
generateJWT jwk uid = do
  Right jwt <- mkJWT jwk ["sub" .= show @Text (DB.fromSqlKey uid)]
  pure $ decodeUtf8 $ toStrict $ JWT.encodeCompact jwt


--------------------------------------------------------------------------------

data LoginUserPayload = LoginUserPayload
  { email    :: Text
  , password :: Text
  }
  deriving (Generic, FromJSON)

checkCredentials :: JWT.JWK -> LoginUserPayload -> DBAction (Maybe UserRecord)
checkCredentials jwk LoginUserPayload{..} = do
  users <- select $ from $
    \u -> do
      where_ (u ^. UserEmail ==. val email)
      where_ (u ^. UserPassword ==. val (hashUserPassword password))
      pure u
  case users of
    [Entity key User{..}] -> do
      userToken <- generateJWT jwk key
      pure $ Just $ UserRecord{userId = DB.fromSqlKey key, ..}
    _ -> pure Nothing


--------------------------------------------------------------------------------

getByKey :: JWT.JWK -> Key User -> DBAction (Maybe UserRecord)
getByKey jwk key = DB.get key >>= traverse mkRecord
  where
    mkRecord User{..} = do
      userToken <- generateJWT jwk key
      pure UserRecord{userId = DB.fromSqlKey key, ..}


--------------------------------------------------------------------------------

data UpdateUserPayload = UpdateUserPayload
  { userUsername :: Maybe Text
  , userEmail    :: Maybe Text
  , userPassword :: Maybe Text
  , userBio      :: Maybe (Maybe Text)
  , userImage    :: Maybe (Maybe Text)
  }
  deriving (Generic)

instance FromJSON UpdateUserPayload where
  parseJSON = genericParseJSON dropPrefixOptions

update :: JWT.JWK -> Key User -> UpdateUserPayload -> DBAction (Maybe UserRecord)
update jwk key UpdateUserPayload{..} = do
  let updates = catMaybes [ UserUsername =?. userUsername
                          , UserEmail    =?. userEmail
                          , UserPassword =?. (hashUserPassword <$> userPassword)
                          , UserBio      =?. userBio
                          , UserImage    =?. userImage
                          ]
  E.update $ \u -> do
    set u updates
    where_ (u ^. UserId ==. val key)
  getByKey jwk key
