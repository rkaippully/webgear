{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Model
  ( withDBConn
  , PrimaryKey (..)

  , User (..)
  , UserM (..)
  , hashUserPassword
  , getUserById
  , getUserByEmail
  , getUserByName
  , createUser
  , updateUser
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, mapReaderT, runReaderT, withReaderT)
import qualified Crypto.Hash as Hash
import Data.Maybe (catMaybes)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Database.Groundhog
import Database.Groundhog.Sqlite (Sqlite, withSqliteConn)
import Entities
import Types


withDBConn :: (Sqlite -> IO a) -> IO a
withDBConn f = withSqliteConn ":memory:" $ \conn -> do
  createSchema conn
  liftIO $ f conn

createSchema :: Sqlite -> IO ()
createSchema = runReaderT migrateAll
  where
    migrateAll = runMigration $ do
      migrate (undefined :: User)

data UserM = UserM
  { useridM   :: Int
  , usernameM :: Maybe Text
  , emailM    :: Maybe Text
  , passwordM :: Maybe Text
  , bioM      :: Maybe (Maybe Text)
  , imageM    :: Maybe (Maybe Text)
  }


hashUserPassword :: Text -> Text
hashUserPassword = pack . show . Hash.hashWith Hash.SHA256 . encodeUtf8

toAppM :: ReaderT Sqlite IO a -> AppM a
toAppM = AppM . mapReaderT liftIO . withReaderT fst

getUserById :: PrimaryKey -> AppM (Maybe User)
getUserById = toAppM . getBy . UserIdKey

getUserByName :: Text -> AppM (Maybe User)
getUserByName = toAppM . getBy . UserNameKey

getUserByEmail :: Text -> AppM (Maybe User)
getUserByEmail = toAppM . getBy . UserEmailKey

createUser :: User -> AppM (Maybe Int)
createUser user = toAppM $ do
  let user' = user{ password = hashUserPassword (password user) }
  insertByAll user' >>= \case
    Left ()  -> pure Nothing
    Right () -> do
      pk <- fmap userid <$> getBy (UserNameKey $ username user)
      pure $ pk >>= unPrimaryKey

updateUser :: UserM -> AppM ()
updateUser UserM{..} = toAppM $ do
  let mkUpdate fld = fmap (fld =.)
      setters = catMaybes [ mkUpdate UsernameField usernameM
                          , mkUpdate EmailField emailM
                          , mkUpdate PasswordField (hashUserPassword <$> passwordM)
                          , mkUpdate BioField bioM
                          , mkUpdate ImageField imageM
                          ]
  update setters $ UseridField ==. PrimaryKey (Just useridM)
