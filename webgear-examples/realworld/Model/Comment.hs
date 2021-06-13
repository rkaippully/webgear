{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE RecordWildCards       #-}
module Model.Comment
  ( CreateCommentPayload (..)
  , CommentRecord (..)
  , create
  , list
  , Model.Comment.delete
  ) where

import Data.Aeson
import Data.Maybe (fromJust)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (getCurrentTime)
import Database.Esqueleto as E
import qualified Database.Persist.Sql as DB
import Model.Common
import Model.Entities
import qualified Model.Profile as Profile
import Relude


newtype CreateCommentPayload = CreateCommentPayload
  { commentBody :: Text }
  deriving (Generic)

instance FromJSON CreateCommentPayload where
  parseJSON = genericParseJSON dropPrefixOptions

data CommentRecord = CommentRecord
  { commentId        :: Int64
  , commentCreatedAt :: UTCTime
  , commentUpdatedAt :: UTCTime
  , commentBody      :: Text
  , commentAuthor    :: Maybe Profile.Profile
  }
  deriving (Generic)

instance ToJSON CommentRecord where
  toJSON = genericToJSON dropPrefixOptions

create :: Key User -> Text -> CreateCommentPayload -> DBAction (Maybe CommentRecord)
create commentAuthor slug CreateCommentPayload{..} =
  getArticleIdBySlug slug >>= traverse doCreate
  where
    doCreate :: Key Article -> DBAction CommentRecord
    doCreate commentArticle = do
      commentCreatedAt <- liftIO getCurrentTime
      let commentUpdatedAt = commentCreatedAt
      commentId <- DB.insert Comment{..}
      fromJust <$> getCommentRecord (Just commentAuthor) commentId

getArticleIdBySlug :: Text -> DBAction (Maybe (Key Article))
getArticleIdBySlug slug = fmap unValue . listToMaybe <$>
  (select $ from $
    \article -> do
      where_ (article ^. ArticleSlug ==. val slug)
      pure $ article ^. ArticleId)

getCommentRecord :: Maybe (Key User) -> Key Comment -> DBAction (Maybe CommentRecord)
getCommentRecord maybeUserId commentId = DB.get commentId >>= traverse mkRecord
  where
    mkRecord :: Comment -> DBAction CommentRecord
    mkRecord Comment{..} = do
      authorProfile <- Profile.getOne maybeUserId commentAuthor
      pure CommentRecord
        { commentId = DB.fromSqlKey commentId
        , commentAuthor = authorProfile
        , ..
        }


--------------------------------------------------------------------------------

list :: Maybe (Key User) -> Text -> DBAction [CommentRecord]
list maybeCurrentUserId slug = do
  commentIds <- select $ from $
    \(article, comment) -> do
      where_ (article ^. ArticleId ==. comment ^. CommentArticle)
      where_ (article ^. ArticleSlug ==. val slug)
      pure $ comment ^. CommentId
  comments <- traverse (getCommentRecord maybeCurrentUserId . unValue) commentIds
  pure $ catMaybes comments


--------------------------------------------------------------------------------

delete :: Key User -> Text -> Key Comment -> DBAction ()
delete authorId slug commentId = E.delete $ from $
  \comment -> do
    where_ (comment ^. CommentId ==. val commentId)
    where_ (comment ^. CommentAuthor ==. val authorId)
    where_ $ exists $ from $
      \article -> do
        where_ (comment ^. CommentArticle ==. article ^. ArticleId)
        where_ (article ^. ArticleSlug ==. val slug)
