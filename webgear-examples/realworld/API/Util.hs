{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module API.Util where

import Control.Lens (view)
import qualified Crypto.JWT as JWT
import Model
import Text.Read (readMaybe)
import Types
import WebGear


type RequiredAuth = JWTAuth' Required "token" AppM () PrimaryKey
type OptionalAuth = JWTAuth' Optional "token" AppM () PrimaryKey

-- Middleware for JWT authentication with "token" scheme
requiredJWTAuth :: RequestMiddleware' AppM req (RequiredAuth : req) a
requiredJWTAuth handler = Kleisli $ \request -> do
  jwk <- askJWK
  let handler' = jwtAuth' JWTAuthConfig{jwkSet = JWT.JWKSet [jwk], ..} handler
  runKleisli handler' request
  where
    jwtAuthRealm = "realworld"
    jwtValidationSettings = JWT.defaultJWTValidationSettings $ const True

    toJWTAttribute :: JWT.ClaimsSet -> AppM (Either () PrimaryKey)
    toJWTAttribute claims = pure $
      case view JWT.claimSub claims >>= readMaybe . unpack . view JWT.string of
        Nothing  -> Left ()
        Just oid -> Right oid
