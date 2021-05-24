{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import qualified API.User as User
import Control.Monad.Reader (runReaderT)
import qualified Crypto.JWT as JWT
import Database.SQLite.Simple (Connection)
import Model (withDBConn)
import Network.HTTP.Types (StdMethod (..))
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Types
import WebGear


--------------------------------------------------------------------------------
-- A medium.com clone app specified by https://github.com/gothinkster/realworld
--------------------------------------------------------------------------------


allRoutes :: Handler' AppM '[] ByteString
allRoutes = do
  -- User APIs
  [route|     POST /api/users       |] User.create
  <|> [route| POST /api/users/login |] User.login
  <|> [route| GET  /api/user        |] User.current
  <|> [route| PUT  /api/user        |] User.update


application :: Connection -> JWT.JWK -> Wai.Application
application conn jwk = toApplication $ transform appToRouter allRoutes
  where
    appToRouter :: AppM a -> Router a
    appToRouter = flip runReaderT (conn, jwk) . unAppM

main :: IO ()
main = withDBConn $ \conn -> do
  jwk <- JWT.genJWK (JWT.RSAGenParam 512)
  Warp.run 3000 (application conn jwk)
