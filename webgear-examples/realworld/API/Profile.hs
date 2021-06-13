{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module API.Profile
  ( getByName
  , follow
  , unfollow
  ) where

import API.Common
import qualified Model.Profile as Model
import Relude
import WebGear


type ProfileResponse = Wrapped "profile" Model.Profile

getByName :: HasTrait (PathVar "username" Text) req => Handler' App req LByteString
getByName = optionalTokenAuth
            $ jsonResponseBody @ProfileResponse
            $ handler
  where
    handler = Kleisli $ \request -> do
      let maybeCurrentUserId = rightToMaybe $ pick @OptionalAuth $ from request
          username = pick @(PathVar "username" Text) $ from request
      maybeProfile <- runDBAction $ Model.getByName maybeCurrentUserId username
      pure $ maybe notFound404 (ok200 . Wrapped) maybeProfile

follow :: HasTrait (PathVar "username" Text) req => Handler' App req LByteString
follow = requiredTokenAuth
         $ jsonResponseBody @ProfileResponse
         $ handler
  where
    handler = Kleisli $ \request -> do
      let currentUserId = pick @RequiredAuth $ from request
          username = pick @(PathVar "username" Text) $ from request
      maybeProfile <- runDBAction $ Model.follow currentUserId username
      pure $ maybe notFound404 (ok200 . Wrapped) maybeProfile


unfollow :: HasTrait (PathVar "username" Text) req => Handler' App req LByteString
unfollow = requiredTokenAuth
           $ jsonResponseBody @ProfileResponse
           $ handler
  where
    handler = Kleisli $ \request -> do
      let currentUserId = pick @RequiredAuth $ from request
          username = pick @(PathVar "username" Text) $ from request
      maybeProfile <- runDBAction $ Model.unfollow currentUserId username
      pure $ maybe notFound404 (ok200 . Wrapped) maybeProfile
