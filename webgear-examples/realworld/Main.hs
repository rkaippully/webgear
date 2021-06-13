{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}
module Main where

import qualified API.Article as Article
import qualified API.Comment as Comment
import API.Common (App (..), AppEnv (..))
import qualified API.Profile as Profile
import qualified API.Tag as Tag
import qualified API.User as User
import qualified Crypto.JWT as JWT
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as LBS
import Data.Pool (Pool)
import Database.Persist.Sql (SqlBackend)
import Model.Common (withDBConnectionPool)
import Network.HTTP.Types (StdMethod (..))
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Relude
import WebGear


--------------------------------------------------------------------------------
-- A medium.com clone app specified by https://github.com/gothinkster/realworld
--------------------------------------------------------------------------------

allRoutes :: Handler' App '[] LByteString
allRoutes =
      [route| POST    /api/users                                       |] User.create
  <|> [route| POST    /api/users/login                                 |] User.login
  <|> [route| GET     /api/user                                        |] User.current
  <|> [route| PUT     /api/user                                        |] User.update
  <|> [route| GET     /api/profiles/username:Text                      |] Profile.getByName
  <|> [route| POST    /api/profiles/username:Text/follow               |] Profile.follow
  <|> [route| DELETE  /api/profiles/username:Text/follow               |] Profile.unfollow
  <|> [route| POST    /api/articles                                    |] Article.create
  <|> [route| GET     /api/articles                                    |] Article.list
  <|> [route| GET     /api/articles/feed                               |] Article.feed
  <|> [route| GET     /api/articles/slug:Text                          |] Article.getBySlug
  <|> [route| PUT     /api/articles/slug:Text                          |] Article.update
  <|> [route| DELETE  /api/articles/slug:Text                          |] Article.delete
  <|> [route| POST    /api/articles/slug:Text/favorite                 |] Article.favorite
  <|> [route| DELETE  /api/articles/slug:Text/favorite                 |] Article.unfavorite
  <|> [route| POST    /api/articles/slug:Text/comments                 |] Comment.create
  <|> [route| GET     /api/articles/slug:Text/comments                 |] Comment.list
  <|> [route| DELETE  /api/articles/slug:Text/comments/commentId:Int64 |] Comment.delete
  <|> [route| GET     /api/tags                                        |] Tag.list

      -- UI resources
  <|> [match| GET     /ui/assets |] serveUIAssets
  <|> [match| GET     /ui        |] serveIndex
  <|> [route| GET     /          |] serveIndex

serveUIAssets :: Handler' App req LByteString
serveUIAssets = serveDir "ui/assets" Nothing

serveIndex :: Handler' App req LByteString
serveIndex = Kleisli $ const $ serveFile "ui/index.html"

application :: Pool SqlBackend -> JWT.JWK -> Wai.Application
application pool jwk = toApplication $ transform appToRouter allRoutes
  where
    appToRouter :: App a -> Router a
    appToRouter = flip runReaderT (AppEnv pool jwk) . unApp

main :: IO ()
main = withDBConnectionPool $ \pool -> do
  jwkBS <- LBS.readFile "realworld.jwk"
  let jwk = either (error . toText) id $ eitherDecode jwkBS
  Warp.run 3000 (application pool jwk)
