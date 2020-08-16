{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DerivingVia    #-}

module Model where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Hashable (Hashable)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import GHC.Generics (Generic)

import qualified Data.HashMap.Strict as HM


--------------------------------------------------------------------------------
-- Model for users
--------------------------------------------------------------------------------
data User = User
  { userId       :: UserId
  , userName     :: Text
  , dateOfBirth  :: Day
  , gender       :: Gender
  , emailAddress :: Text
  }
  deriving (Generic, FromJSON, ToJSON)

newtype UserId = UserId Int
  deriving (Eq, FromJSON, ToJSON, Hashable) via Int

data Gender = Male | Female | OtherGender
  deriving (Generic, FromJSON, ToJSON)


--------------------------------------------------------------------------------
-- An in-memory store and associated operations for users
--------------------------------------------------------------------------------
newtype UserStore = UserStore (IORef (HM.HashMap UserId User))

newStore :: MonadIO m => m UserStore
newStore = UserStore <$> liftIO (newIORef HM.empty)

addUser :: MonadIO m => UserStore -> User -> m ()
addUser (UserStore ref) user = liftIO $ modifyIORef ref (HM.insert (userId user) user)

lookupUser :: MonadIO m => UserStore -> UserId -> m (Maybe User)
lookupUser (UserStore ref) uid = liftIO (HM.lookup uid <$> readIORef ref)

removeUser :: MonadIO m => UserStore -> UserId -> m Bool
removeUser store@(UserStore ref) uid = liftIO $ do
  u <- lookupUser store uid
  modifyIORef ref (HM.delete uid)
  pure $ isJust u
