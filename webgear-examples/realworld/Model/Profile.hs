{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}
module Model.Profile
  ( Profile (..)
  , getOne
  , getByName
  , follow
  , unfollow
  )
  where

import Control.Exception.Safe (catch, throw)
import Data.Aeson (ToJSON (..), genericToJSON)
import Database.Esqueleto
import qualified Database.Persist as DB
import Database.Sqlite (Error (..), SqliteException (..))
import Model.Common
import Model.Entities
import Relude


data Profile = Profile
  { userUsername  :: Text
  , userBio       :: Maybe Text
  , userImage     :: Maybe Text
  , userFollowing :: Bool
  }
  deriving (Generic)

instance ToJSON Profile where
  toJSON = genericToJSON dropPrefixOptions

getOne :: Maybe (Key User)  -- ^ current user (if any)
       -> Key User          -- ^ user to get profile of
       -> DBAction (Maybe Profile)
getOne maybeFollowerKey followeeKey =
  findProfile maybeFollowerKey $
    \user -> user ^. UserId ==. val followeeKey

findProfile :: Maybe (Key User)
            -> (SqlExpr (Entity User) -> SqlExpr (Value Bool))
            -> DBAction (Maybe Profile)
findProfile maybeFollowerKey selector = do
  maybeResult <- fmap listToMaybe <$>
    select $ from $
    \user -> do
      where_ (selector user)
      pure (user ^. UserId, user ^. UserUsername, user ^. UserBio, user ^. UserImage)
  traverse mkProfile maybeResult
  where
    mkProfile (Value userId, Value userUsername, Value userBio, Value userImage) = do
      maybeFollowing <- traverse (isFollowing userId) maybeFollowerKey
      pure Profile{userFollowing = fromMaybe False maybeFollowing, ..}

    isFollowing :: Key User -> Key User -> DBAction Bool
    isFollowing followeeKey followerKey = do
      followCount <- select $ from $
                     \flw -> do
                       where_ (flw ^. FollowFollower ==. val followerKey)
                       where_ (flw ^. FollowFollowee ==. val followeeKey)
                       pure (countRows @Int)
      pure $ followCount == [Value 1]


--------------------------------------------------------------------------------

getByName :: Maybe (Key User)  -- ^ current user (if any)
          -> Text              -- ^ username to get profile of
          -> DBAction (Maybe Profile)
getByName maybeFollowerKey username =
  findProfile maybeFollowerKey $
    \user -> user ^. UserUsername ==. val username


--------------------------------------------------------------------------------

follow :: Key User -> Text -> DBAction (Maybe Profile)
follow followerKey followeeUsername = getUserIdByName followeeUsername >>= \case
  Nothing -> pure Nothing
  Just followeeKey -> do
    let flw = Follow{followFollower = followerKey, followFollowee = followeeKey}
    let handleDBError :: SqliteException -> DBAction ()
        handleDBError e | seError e == ErrorConstraint = pure ()
                        | otherwise = throw e
    DB.insert_ flw `catch` handleDBError
    getOne (Just followerKey) followeeKey

getUserIdByName :: Text -> DBAction (Maybe (Key User))
getUserIdByName name = do
  result <- select $ from $
            \user -> do
              where_ (user ^. UserUsername ==. val name)
              pure (user ^. UserId)
  pure $ unValue <$> listToMaybe result


--------------------------------------------------------------------------------

unfollow :: Key User -> Text -> DBAction (Maybe Profile)
unfollow followerKey followeeUsername = getUserIdByName followeeUsername >>= \case
  Nothing -> pure Nothing
  Just followeeKey -> do
    DB.deleteBy (UniqueFollow followerKey followeeKey)
    getOne (Just followerKey) followeeKey
