{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeApplications      #-}
module API.Article
  ( create
  , getBySlug
  , update
  , delete
  , list
  , feed
  , favorite
  , unfavorite
  ) where

import API.Common
import Control.Exception.Safe (catch)
import Data.Aeson (ToJSON)
import qualified Database.Sqlite as DB
import qualified Model.Article as Model
import Relude
import WebGear hiding (length)


type CreateArticleRequest = Wrapped "article" Model.CreateArticlePayload
type ArticleResponse = Wrapped "article" Model.ArticleRecord

create :: Handler' App req LByteString
create = requiredTokenAuth
         $ jsonRequestBody @CreateArticleRequest
         $ jsonResponseBody @ArticleResponse
         $ handler
  where
    handler = Kleisli $ \request -> do
      let currentUserId = pick @RequiredAuth $ from request
          articlePayload = pick @(JSONBody CreateArticleRequest) $ from request
      let doCreate = do
            article <- runDBAction $ Model.create currentUserId (unwrap articlePayload)
            pure $ ok200 $ Wrapped article
      doCreate `catch` handleDBError

handleDBError :: DB.SqliteException -> App (Response a)
handleDBError e | DB.seError e == DB.ErrorConstraint = errorResponse $ badRequest400 "Article already exists"
                | otherwise = errorResponse $ internalServerError500 $ fromString $ show e


--------------------------------------------------------------------------------

getBySlug :: HasTrait (PathVar "slug" Text) req => Handler' App req LByteString
getBySlug = optionalTokenAuth
            $ jsonResponseBody @ArticleResponse
            $ handler
  where
    handler = Kleisli $ \request -> do
      let maybeCurrentUserId = rightToMaybe $ pick @OptionalAuth $ from request
          slug = pick @(PathVar "slug" Text) $ from request
      maybeArticle <- runDBAction $ Model.getArticleBySlug maybeCurrentUserId slug
      pure $ maybe notFound404 (ok200 . Wrapped) maybeArticle


--------------------------------------------------------------------------------

type UpdateArticleRequest = Wrapped "article" Model.UpdateArticlePayload

update :: HasTrait (PathVar "slug" Text) req => Handler' App req LByteString
update = requiredTokenAuth
         $ jsonRequestBody @UpdateArticleRequest
         $ jsonResponseBody @ArticleResponse
         $ handler
  where
    handler = Kleisli $ \request -> do
      let userId = pick @RequiredAuth $ from request
          updatePayload = pick @(JSONBody UpdateArticleRequest) $ from request
          articleSlug = pick @(PathVar "slug" Text) $ from request

      runDBAction (Model.getArticleIdAndAuthorBySlug articleSlug) >>= \case
        Nothing -> pure notFound404
        Just (articleId, authorId)
          | authorId /= userId -> errorResponse $ forbidden403 "Permission denied"
          | otherwise -> do
              let doUpdate = do
                    maybeArticle <- runDBAction $ Model.update authorId articleId (unwrap updatePayload)
                    pure $ maybe notFound404 (ok200 . Wrapped) maybeArticle
              doUpdate `catch` handleDBError


--------------------------------------------------------------------------------

delete :: HasTrait (PathVar "slug" Text) req => Handler' App req LByteString
delete = requiredTokenAuth handler
  where
    handler = Kleisli $ \request -> do
      let currentUserId = pick @RequiredAuth $ from request
          slug = pick @(PathVar "slug" Text) $ from request
      runDBAction $ Model.delete currentUserId slug
      pure noContent204


--------------------------------------------------------------------------------

data ArticleListResponse = ArticleListResponse
  { articles      :: [Model.ArticleRecord]
  , articlesCount :: Int
  }
  deriving (Generic, ToJSON)

list :: Handler' App req LByteString
list = optionalTokenAuth
       $ optionalQueryParam @"tag" @Text
       $ optionalQueryParam @"author" @Text
       $ optionalQueryParam @"favorited" @Text
       $ optionalQueryParam @"limit" @Int64
       $ optionalQueryParam @"offset" @Int64
       $ jsonResponseBody @ArticleListResponse
       $ handler
  where
    handler = Kleisli $ \request -> do
      let maybeCurrentUserId = rightToMaybe $ pick @OptionalAuth $ from request
          maybeTag = pick @(QueryParam' Optional Strict "tag" Text) $ from request
          maybeAuthorName = pick @(QueryParam' Optional Strict "author" Text) $ from request
          maybeFavoritedBy = pick @(QueryParam' Optional Strict "favorited" Text) $ from request
          listLimit = fromMaybe 20 $ pick @(QueryParam' Optional Strict "limit" Int64) $ from request
          listOffset = fromMaybe 0 $ pick @(QueryParam' Optional Strict "offset" Int64) $ from request

      articles <- runDBAction $ Model.articleList Model.ArticleListInput{..}
      pure $ ok200 $ ArticleListResponse articles (length articles)


--------------------------------------------------------------------------------

feed :: Handler' App req LByteString
feed = requiredTokenAuth
       $ optionalQueryParam @"limit" @Int64
       $ optionalQueryParam @"offset" @Int64
       $ jsonResponseBody @ArticleListResponse
       $ handler
  where
    handler = Kleisli $ \request -> do
      let currentUserId = pick @RequiredAuth $ from request
          listLimit = fromMaybe 20 $ pick @(QueryParam' Optional Strict "limit" Int64) $ from request
          listOffset = fromMaybe 0 $ pick @(QueryParam' Optional Strict "offset" Int64) $ from request

      articles <- runDBAction $ Model.articleFeed Model.ArticleFeedInput{..}
      pure $ ok200 $ ArticleListResponse articles (length articles)


--------------------------------------------------------------------------------

favorite :: HasTrait (PathVar "slug" Text) req => Handler' App req LByteString
favorite = requiredTokenAuth
           $ jsonResponseBody @ArticleResponse
           $ handler
  where
    handler = Kleisli $ \request -> do
      let currentUserId = pick @RequiredAuth $ from request
          slug = pick @(PathVar "slug" Text) $ from request
      maybeArticle <- runDBAction $ Model.favorite currentUserId slug
      pure $ maybe notFound404 (ok200 . Wrapped) maybeArticle


--------------------------------------------------------------------------------

unfavorite :: HasTrait (PathVar "slug" Text) req => Handler' App req LByteString
unfavorite = requiredTokenAuth
             $ jsonResponseBody @ArticleResponse
             $ handler
  where
    handler = Kleisli $ \request -> do
      let currentUserId = pick @RequiredAuth $ from request
          slug = pick @(PathVar "slug" Text) $ from request
      maybeArticle <- runDBAction $ Model.unfavorite currentUserId slug
      pure $ maybe notFound404 (ok200 . Wrapped) maybeArticle
