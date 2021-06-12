{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
module API.Tag
  ( list
  ) where


import API.Common
import qualified Model.Tag as Model
import Relude
import WebGear


list :: Handler' App req LByteString
list = jsonResponseBody @(Wrapped "tags" [Text]) handler
  where
    handler = Kleisli $ \_request -> do
      tags <- runDBAction Model.list
      pure $ ok200 $ Wrapped tags
