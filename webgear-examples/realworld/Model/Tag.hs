module Model.Tag
  ( list
  ) where

import Database.Esqueleto
import Model.Common
import Model.Entities
import Relude


list :: DBAction [Text]
list = fmap unValue <$>
  -- select only tags used in articles
  (select $ from $
   \(tag, articleTag) -> do
     where_ (tag ^. TagId ==. articleTag ^. ArticleTagTagid)
     pure $ tag ^. TagName)
