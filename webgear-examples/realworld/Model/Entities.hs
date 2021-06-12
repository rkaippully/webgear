{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module Model.Entities where

import Data.Time.Clock (UTCTime)
import Database.Esqueleto
import Database.Persist.TH
import Relude


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  username Text
  email    Text
  password Text
  bio      Text Maybe
  image    Text Maybe
  UniqueUsername username
  UniqueEmail    email

Article
  slug        Text
  title       Text
  description Text
  body        Text
  createdAt   UTCTime
  updatedAt   UTCTime
  author      UserId
  UniqueSlug  slug

ArticleTag
  tagid      TagId
  articleid  ArticleId
  ArticlesWithTag tagid articleid
  TagsOfArticle articleid tagid

Tag
  name Text
  UniqueTagName name

Comment
  createdAt UTCTime
  updatedAt UTCTime
  body      Text
  article   ArticleId
  author    UserId

Favorite
  userid    UserId
  articleid ArticleId
  UniqueFavorite articleid userid

Follow
  follower UserId
  followee UserId
  UniqueFollow follower followee
|]
