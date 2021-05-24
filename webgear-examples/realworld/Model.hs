{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

module Model
  ( withDBConn
  , PrimaryKey (..)

  , UserT (..)
  , User
  , UserM
  , hashUserPassword
  , getUserById
  , getUserByEmail
  , getUserByName
  , createUser
  , updateUser
  ) where

import Control.Exception.Safe (MonadThrow, catch, throw)
import Control.Monad.IO.Class (liftIO)
import qualified Crypto.Hash as Hash
import Data.Aeson (FromJSON, ToJSON)
import Data.Functor.Identity (Identity)
import Data.Int (Int64)
import Data.Text (Text, intercalate, pack)
import Data.Text.Encoding (encodeUtf8)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.ToField
import GHC.Generics (Generic)
import Types


withDBConn :: (Connection -> IO a) -> IO a
withDBConn f = withConnection ":memory:" $ \conn -> do
  createSchema conn
  liftIO $ f conn

createSchema :: Connection -> IO ()
createSchema conn = do
  execute_ conn "CREATE TABLE users (oid INTEGER PRIMARY KEY AUTOINCREMENT, username TEXT NOT NULL UNIQUE, email TEXT NOT NULL UNIQUE, password TEXT NOT NULL, bio TEXT, image TEXT)"

newtype PrimaryKey = PrimaryKey Int64
  deriving newtype (FromJSON, ToJSON, FromField, ToField, Show, Read, Eq, Ord, Enum)

type family Field f t where
  Field Identity t = t
  Field Maybe t = Maybe t

data UserT f = User
  { key      :: PrimaryKey
  , username :: Field f Text
  , email    :: Field f Text
  , password :: Field f Text
  , bio      :: Field f (Maybe Text)
  , image    :: Field f (Maybe Text)
  }
  deriving (Generic)

type User = UserT Identity
type UserM = UserT Maybe

deriving instance FromJSON User
deriving instance ToJSON User
deriving instance FromJSON UserM
deriving instance ToJSON UserM

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow User where
  toRow User{..} = [toField username, toField email, toField password, toField bio, toField image]

hashUserPassword :: Text -> Text
hashUserPassword = pack . show . Hash.hashWith Hash.SHA256 . encodeUtf8

getUserById :: PrimaryKey -> AppM (Maybe User)
getUserById (PrimaryKey oid) = do
  conn <- askConnection
  res <- liftIO $ query conn "SELECT * FROM users where oid = ?" (Only oid)
  pure $ case res of
           [u] -> Just u
           _   -> Nothing

getUserByEmail :: Text -> AppM (Maybe User)
getUserByEmail email = do
  conn <- askConnection
  res <- liftIO $ query conn "SELECT * FROM users where email = ?" (Only email)
  pure $ case res of
           [u] -> Just u
           _   -> Nothing

getUserByName :: Text -> AppM (Maybe User)
getUserByName name = do
  conn <- askConnection
  res <- liftIO $ query conn "SELECT * FROM users where username = ?" (Only name)
  pure $ case res of
           [u] -> Just u
           _   -> Nothing

createUser :: User -> AppM (Either SQLError PrimaryKey)
createUser user = do
  conn <- askConnection
  let user' = user{ password = hashUserPassword (password user) }

  liftIO $ do {
    execute conn "INSERT INTO users (username, email, password, bio, image) VALUES (?,?,?,?,?)" user';
    Right . PrimaryKey <$> lastInsertRowId conn
    } `catch` onConstraintError (pure . Left)

onConstraintError :: MonadThrow m => (SQLError -> m a) -> SQLError -> m a
onConstraintError f e | sqlError e == ErrorConstraint = f e
                      | otherwise = throw e

updateUser :: UserM -> AppM (Either SQLError (Maybe User))
updateUser User{..} = do
  let specified :: [(a, Maybe SQLData)] -> [(a, SQLData)]
      specified xs = [(x, d) | (x, Just d) <- xs]

      fields = specified [ ("username = ?", toField <$> username)
                         , ("email = ?", toField <$> email)
                         , ("password = ?", toField . hashUserPassword <$> password)
                         , ("bio = ?", toField <$> bio)
                         , ("image = ?", toField <$> image)
                         ]

      names = Query $ intercalate "," $ map fst fields
      values = map snd fields

      updateAndGet conn = do
        liftIO $ execute conn ("UPDATE users SET " <> names <> " WHERE oid = ?") (values ++ [toField key])
        Right <$> getUserById key

  conn <- askConnection
  updateAndGet conn `catch` onConstraintError (pure . Left)
