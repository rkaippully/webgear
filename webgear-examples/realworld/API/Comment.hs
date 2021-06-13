{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module API.Comment
  ( create
  , list
  , API.Comment.delete
  ) where

import API.Common
import qualified Database.Persist.Sql as DB
import qualified Model.Comment as Model
import Relude
import WebGear


type CreateCommentRequest = Wrapped "comment" Model.CreateCommentPayload
type CommentResponse = Wrapped "comment" Model.CommentRecord

create :: HasTrait (PathVar "slug" Text) req => Handler' App req LByteString
create = requiredTokenAuth
         $ jsonRequestBody @CreateCommentRequest
         $ jsonResponseBody @CommentResponse
         $ handler
  where
    handler = Kleisli $ \request -> do
      let currentUserId = pick @RequiredAuth $ from request
          slug = pick @(PathVar "slug" Text) $ from request
          payload = pick @(JSONBody CreateCommentRequest) $ from request
      maybeComment <- runDBAction $ Model.create currentUserId slug (unwrap payload)
      pure $ maybe notFound404 (ok200 . Wrapped) maybeComment


--------------------------------------------------------------------------------

type CommentListResponse = Wrapped "comments" [Model.CommentRecord]

list :: HasTrait (PathVar "slug" Text) req => Handler' App req LByteString
list = optionalTokenAuth
       $ jsonResponseBody @CommentListResponse
       $ handler
  where
    handler = Kleisli $ \request -> do
      let maybeCurrentUserId = rightToMaybe $ pick @OptionalAuth $ from request
          slug = pick @(PathVar "slug" Text) $ from request
      comments <- runDBAction $ Model.list maybeCurrentUserId slug
      pure $ ok200 $ Wrapped comments


--------------------------------------------------------------------------------

delete :: HaveTraits [PathVar "slug" Text, PathVar "commentId" Int64] req
       => Handler' App req LByteString
delete = requiredTokenAuth handler
  where
    handler = Kleisli $ \request -> do
      let currentUserId = pick @RequiredAuth $ from request
          slug = pick @(PathVar "slug" Text) $ from request
          commentId = pick @(PathVar "commentId" Int64) $ from request
      runDBAction $ Model.delete currentUserId slug (DB.toSqlKey commentId)
      pure noContent204

