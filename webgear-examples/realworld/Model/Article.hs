{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
module Model.Article
  ( CreateArticlePayload (..)
  , ArticleRecord (..)
  , create
  , getArticleBySlug
  , getArticleIdAndAuthorBySlug
  , UpdateArticlePayload (..)
  , Model.Article.update
  , Model.Article.delete
  , ArticleListInput (..)
  , articleList
  , ArticleFeedInput (..)
  , articleFeed
  , favorite
  , unfavorite
  ) where

import Control.Exception.Safe (catch, throw)
import Data.Aeson
import Data.Char (isSpace)
import Data.Maybe (fromJust)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (getCurrentTime)
import Database.Esqueleto as E
import qualified Database.Persist.Sql as DB
import Database.Sqlite (Error (..), SqliteException (..))
import Model.Common
import Model.Entities
import qualified Model.Profile as Profile
import qualified Network.URI.Encode as URIEncode
import Relude
import System.Random (randomIO)


data CreateArticlePayload = CreateArticlePayload
  { articleTitle       :: Text
  , articleDescription :: Text
  , articleBody        :: Text
  , articleTagList     :: Maybe [Text]
  }
  deriving (Generic)

instance FromJSON CreateArticlePayload where
  parseJSON = genericParseJSON dropPrefixOptions

data ArticleRecord = ArticleRecord
  { articleSlug           :: Text
  , articleTitle          :: Text
  , articleDescription    :: Text
  , articleBody           :: Text
  , articleTagList        :: [Text]
  , articleCreatedAt      :: UTCTime
  , articleUpdatedAt      :: UTCTime
  , articleFavorited      :: Bool
  , articleFavoritesCount :: Int
  , articleAuthor         :: Maybe Profile.Profile
  }
  deriving (Generic)

instance ToJSON ArticleRecord where
  toJSON = genericToJSON dropPrefixOptions

create :: Key User -> CreateArticlePayload -> DBAction ArticleRecord
create userId CreateArticlePayload{..} = do
  articleCreatedAt <- liftIO getCurrentTime
  let articleUpdatedAt = articleCreatedAt
      articleAuthor = userId
      tags = fromMaybe [] articleTagList
  articleSlug <- slugify articleTitle

  articleId <- DB.insert Article{..}
  forM_ tags $ \tag -> do
    tagId <- DB.entityKey <$> DB.upsert (Tag tag) []
    DB.insert $ ArticleTag tagId articleId

  fromJust <$> getArticleRecord (Just userId) articleId

slugify :: MonadIO m => Text -> m Text
slugify s = liftIO $ do
  num <- randomIO
  let suffix = "-" <> show (num :: Word64)
  pure $ (<> suffix)
       $ Text.take 255
       $ Text.filter URIEncode.isAllowed
       $ Text.map (\c -> if isSpace c then '-' else c)
       $ Text.toLower s

getArticleRecord :: Maybe (Key User) -> Key Article -> DBAction (Maybe ArticleRecord)
getArticleRecord maybeUserId articleId = DB.getEntity articleId >>= traverse (mkRecord maybeUserId)

mkRecord :: Maybe (Key User) -> Entity Article -> DBAction ArticleRecord
mkRecord maybeUserId (Entity articleId Article{..}) = do
  articleTagList <- map unValue <$>
    (select $ from $
      \(articleTag, tag) -> do
        where_ (articleTag ^. ArticleTagArticleid ==. val articleId)
        where_ (articleTag ^. ArticleTagTagid ==. tag ^. TagId)
        pure (tag ^. TagName))

  favoritedUsers <- map unValue <$>
    (select $ from $
      \fav -> do
        where_ (fav ^. FavoriteArticleid ==. val articleId)
        pure $ fav ^. FavoriteUserid)
  let articleFavorited = maybe False (`elem` favoritedUsers) maybeUserId
  let articleFavoritesCount = length favoritedUsers
  authorProfile <- Profile.getOne maybeUserId articleAuthor

  pure $ ArticleRecord{articleAuthor = authorProfile, ..}


--------------------------------------------------------------------------------

getArticleBySlug :: Maybe (Key User) -> Text -> DBAction (Maybe ArticleRecord)
getArticleBySlug maybeUserId slug =
  DB.getBy (UniqueSlug slug) >>= traverse (mkRecord maybeUserId)


--------------------------------------------------------------------------------

getArticleIdAndAuthorBySlug :: Text -> DBAction (Maybe (Key Article, Key User))
getArticleIdAndAuthorBySlug slug = do
  maybeResult <- listToMaybe <$>
    (select $ from $
     \article -> do
       where_ (article ^. ArticleSlug ==. val slug)
       pure (article ^. ArticleId, article ^. ArticleAuthor))
  pure $ fmap (\(Value aid, Value uid) -> (aid, uid)) maybeResult


--------------------------------------------------------------------------------

data UpdateArticlePayload = UpdateArticlePayload
  { articleTitle       :: Maybe Text
  , articleDescription :: Maybe Text
  , articleBody        :: Maybe Text
  }
  deriving (Generic)

instance FromJSON UpdateArticlePayload where
  parseJSON = genericParseJSON dropPrefixOptions

update :: Key User  -- ^ author
       -> Key Article
       -> UpdateArticlePayload
       -> DBAction (Maybe ArticleRecord)
update authorId articleId UpdateArticlePayload{..} = do
  newSlug <- traverse slugify articleTitle
  now <- liftIO getCurrentTime
  let updates = catMaybes [ ArticleTitle       =?. articleTitle
                          , ArticleDescription =?. articleDescription
                          , ArticleBody        =?. articleBody
                          , ArticleSlug        =?. newSlug
                          , ArticleUpdatedAt   =?. Just now
                          ]
  E.update $ \article -> do
    set article updates
    where_ (article ^. ArticleId ==. val articleId)
    where_ (article ^. ArticleAuthor ==. val authorId)

  getArticleRecord (Just authorId) articleId


--------------------------------------------------------------------------------

delete :: Key User -> Text -> DBAction ()
delete userId slug = do
  let matchSlugAndAuthor article = do
        where_ (article ^. ArticleSlug ==. val slug)
        where_ (article ^. ArticleAuthor ==. val userId)

  E.delete $ from $
    \articleTag ->
      where_ $ exists $ from $
        \article -> do
          where_ (article ^. ArticleId ==. articleTag ^. ArticleTagArticleid)
          matchSlugAndAuthor article

  E.delete $ from $
    \comment ->
      where_ $ exists $ from $
        \article -> do
          where_ (article ^. ArticleId ==. comment ^. CommentArticle)
          matchSlugAndAuthor article

  E.delete $ from $
    \fav ->
      where_ $ exists $ from $
        \article -> do
          where_ (article ^. ArticleId ==. fav ^. FavoriteArticleid)
          matchSlugAndAuthor article

  E.delete $ from matchSlugAndAuthor


--------------------------------------------------------------------------------

data ArticleListInput = ArticleListInput
  { maybeCurrentUserId :: Maybe (Key User)
  , maybeTag           :: Maybe Text
  , maybeAuthorName    :: Maybe Text
  , maybeFavoritedBy   :: Maybe Text
  , listLimit          :: Int64
  , listOffset         :: Int64
  }

articleList :: ArticleListInput -> DBAction [ArticleRecord]
articleList ArticleListInput{..} = do
  let filterTag article =
        case maybeTag of
          Nothing -> val True
          Just "" -> val True
          Just aTag -> exists $
            from $ \(articleTag, tag) -> do
              where_ (article ^. ArticleId ==. articleTag ^. ArticleTagArticleid)
              where_ (articleTag ^. ArticleTagTagid ==. tag ^. TagId)
              where_ (tag ^. TagName ==. val aTag)

      filterAuthor article =
        case maybeAuthorName of
          Nothing -> val True
          Just "" -> val True
          Just authorName -> exists $
            from $ \user -> do
              where_ (article ^. ArticleAuthor ==. user ^. UserId)
              where_ (user ^. UserUsername  ==. val authorName)

      filterFavorite article =
        case maybeFavoritedBy of
          Nothing -> val True
          Just "" -> val True
          Just favUserName -> exists $
            from $ \(fav, user) -> do
              where_ (article ^. ArticleId ==. fav ^. FavoriteArticleid)
              where_ (fav ^. FavoriteUserid ==. user ^. UserId)
              where_ (user ^. UserUsername ==. val favUserName)

  articleIds <- select $ from $
    \article -> do
      where_ (filterTag article)
      where_ (filterAuthor article)
      where_ (filterFavorite article)
      orderBy [desc $ article ^. ArticleUpdatedAt]
      limit listLimit
      offset listOffset
      pure $ article ^. ArticleId

  articles <- traverse (getArticleRecord maybeCurrentUserId . unValue) articleIds
  pure $ catMaybes articles


--------------------------------------------------------------------------------

data ArticleFeedInput = ArticleFeedInput
  { currentUserId :: Key User
  , listLimit     :: Int64
  , listOffset    :: Int64
  }

articleFeed :: ArticleFeedInput -> DBAction [ArticleRecord]
articleFeed ArticleFeedInput{..} = do
  articleIds <- select $ from $
    \(article, follow) -> do
      where_ (follow ^. FollowFollower ==. val currentUserId)
      where_ (follow ^. FollowFollowee ==. article ^. ArticleAuthor)
      orderBy [desc $ article ^. ArticleUpdatedAt]
      limit listLimit
      offset listOffset
      pure $ article ^. ArticleId

  articles <- traverse (getArticleRecord (Just currentUserId) . unValue) articleIds
  pure $ catMaybes articles


--------------------------------------------------------------------------------

favorite :: Key User -> Text -> DBAction (Maybe ArticleRecord)
favorite userId slug = getArticleIdAndAuthorBySlug slug >>= \case
  Nothing -> pure Nothing
  Just (articleId, _) -> do
    let fav = Favorite{favoriteUserid = userId, favoriteArticleid = articleId}
    let handleDBError :: SqliteException -> DBAction ()
        handleDBError e | seError e == ErrorConstraint = pure ()
                        | otherwise = throw e
    DB.insert_ fav `catch` handleDBError
    getArticleRecord (Just userId) articleId


--------------------------------------------------------------------------------

unfavorite :: Key User -> Text -> DBAction (Maybe ArticleRecord)
unfavorite userId slug = getArticleIdAndAuthorBySlug slug >>= \case
  Nothing -> pure Nothing
  Just (articleId, _) -> do
    DB.deleteBy (UniqueFavorite articleId userId)
    getArticleRecord (Just userId) articleId
