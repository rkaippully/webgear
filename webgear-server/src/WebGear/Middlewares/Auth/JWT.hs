{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Copyright        : (c) Raghu Kaippully, 2020-2021
-- License          : MPL-2.0
-- Maintainer       : rkaippully@gmail.com
--
-- JWT authentication support.
--
module WebGear.Middlewares.Auth.JWT
  ( JWTAuth'
  , JWTAuth
  , JWTAuthConfig (..)
  , Realm (..)
  , JWTAuthError (..)
  , jwtAuth
  , optionalJWTAuth
  , jwtAuth'
  , optionalJWTAuth'
  , mkJWT
  ) where

import Control.Arrow (Kleisli (..))
import Control.Monad ((>=>))
import Control.Monad.Except (MonadError (throwError), lift, runExceptT, withExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Time (MonadTime)
import qualified Crypto.JWT as JWT
import Data.Aeson (Object, Result (..), Value (..), fromJSON)
import Data.ByteString.Lazy (fromStrict)
import Data.Proxy (Proxy (..))
import Data.String (fromString)
import Data.Text.Lazy ()
import Data.Void (Void, absurd)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import WebGear.Middlewares.Auth.Util (AuthToken (..), AuthorizationHeader, Realm (..),
                                      authorizationHeader, respondUnauthorized)
import WebGear.Modifiers (Existence (..))
import WebGear.Trait (HasTrait (..), Linked, Trait (..), pick, transcribe)
import WebGear.Types (MonadRouter (..), Request, RequestMiddleware', Response, forbidden403)


-- | Trait for JWT authentication with a bearer token:
-- https://tools.ietf.org/html/rfc6750
--
-- This trait supports a custom scheme instead of the standard
-- "Bearer" scheme.
data JWTAuth' (x :: Existence) (scheme :: Symbol) m e a = JWTAuth'
  { jwtValidationSettings :: JWT.JWTValidationSettings
  , jwkSet                :: JWT.JWKSet
  , toJWTAttribute        :: JWT.ClaimsSet -> m (Either e a)
  }

type JWTAuth = JWTAuth' Required "Bearer"

-- | Configuration settings for JWT authentication
data JWTAuthConfig m e a = JWTAuthConfig
  { jwtAuthRealm          :: Realm
  , jwtValidationSettings :: JWT.JWTValidationSettings
  , jwkSet                :: JWT.JWKSet
  , toJWTAttribute        :: JWT.ClaimsSet -> m (Either e a)
  }

data JWTAuthError e = JWTAuthHeaderMissing
                    | JWTAuthSchemeMismatch
                    | JWTAuthTokenBadFormat JWT.JWTError
                    | JWTAuthAttributeError e
                    deriving (Eq, Show)

parseJWT :: AuthToken scheme -> Either JWT.JWTError JWT.SignedJWT
parseJWT AuthToken{..} = JWT.decodeCompact $ fromStrict authToken

instance (HasTrait (AuthorizationHeader scheme) ts, MonadIO m, MonadTime m) => Trait (JWTAuth' Required scheme m e a) ts Request m where
  type Attribute (JWTAuth' Required scheme m e a) Request = a
  type Absence (JWTAuth' Required scheme m e a) Request = JWTAuthError e

  tryLink :: JWTAuth' Required scheme m e a
          -> Linked ts Request
          -> m (Either (JWTAuthError e) a)
  tryLink JWTAuth'{..} r =
    case pick @(AuthorizationHeader scheme) (from r) of
      Nothing            -> pure $ Left JWTAuthHeaderMissing
      Just (Left _)      -> pure $ Left JWTAuthSchemeMismatch
      Just (Right token) -> either (pure . Left . JWTAuthTokenBadFormat) validateJWT (parseJWT token)
    where
      validateJWT :: JWT.SignedJWT -> m (Either (JWTAuthError e) a)
      validateJWT jwt = runExceptT $ do
        claims <- withExceptT JWTAuthTokenBadFormat $ JWT.verifyClaims jwtValidationSettings jwkSet jwt
        lift (toJWTAttribute claims) >>= either (throwError . JWTAuthAttributeError) pure

instance (HasTrait (AuthorizationHeader scheme) ts, MonadIO m, MonadTime m) => Trait (JWTAuth' Optional scheme m e a) ts Request m where
  type Attribute (JWTAuth' Optional scheme m e a) Request = Either (JWTAuthError e) a
  type Absence (JWTAuth' Optional scheme m e a) Request = Void

  tryLink :: JWTAuth' Optional scheme m e a
          -> Linked ts Request
          -> m (Either Void (Either (JWTAuthError e) a))
  tryLink JWTAuth'{..} r = Right <$> tryLink (JWTAuth'{..} :: JWTAuth' Required scheme m e a) r


-- | Middleware to add JWT authentication protection for a
-- handler. Expects the JWT to be available via a standard bearer
-- authorization header in the format:
--
-- > Authorization: Bearer <jwt>
--
-- Example usage:
--
-- > jwtAuth cfg handler
--
-- This middleware returns a 401 response if the authorization header
-- is missing or formatted incorrectly. It returns a 403 response if
-- the JWT is invalid.
jwtAuth :: forall m req e t a. (MonadRouter m, MonadIO m, MonadTime m)
        => JWTAuthConfig m e t
        -> RequestMiddleware' m req (JWTAuth m e t : req) a
jwtAuth = jwtAuth' @"Bearer"

-- | Middleware to add optional JWT authentication protection for a
-- handler. Expects the JWT to be available via a standard bearer
-- authorization header in the format:
--
-- > Authorization: Bearer <jwt>
--
-- Example usage:
--
-- > optionalJWTAuth cfg handler
--
-- This middleware will not fail if authorization credentials are
-- invalid or missing in the request. Instead the trait attribute is
-- of type Either 'JWTAuthError' 'JWT.ClaimsSet' so that the handler
-- can process the authentication error appropriately.
optionalJWTAuth :: forall m req e t a. (MonadRouter m, MonadIO m, MonadTime m)
                => JWTAuthConfig m e t
                -> RequestMiddleware' m req (JWTAuth' Optional "Bearer" m e t : req) a
optionalJWTAuth = optionalJWTAuth' @"Bearer"

-- | Middleware to add JWT authentication protection for a
-- handler. Expects the JWT to be available via an authorization
-- header in the format:
--
-- > Authorization: <scheme> <jwt>
--
-- Example usage:
--
-- > jwtAuth' @"<scheme>" cfg handler
--
-- This middleware returns a 401 response if the authorization header
-- is missing or formatted incorrectly. It returns a 403 response if
-- the JWT is invalid.
jwtAuth' :: forall scheme m req e t a. (MonadRouter m, MonadIO m, MonadTime m, KnownSymbol scheme)
         => JWTAuthConfig m e t
         -> RequestMiddleware' m req (JWTAuth' Required scheme m e t : req) a
jwtAuth' JWTAuthConfig{..} handler = authorizationHeader @scheme $ Kleisli $
  transcribe JWTAuth'{..}  >=> either mkError (runKleisli handler)
  where
    mkError :: JWTAuthError e -> m (Response a)
    mkError (JWTAuthTokenBadFormat e) = errorResponse $ forbidden403 $ fromString $ show e
    mkError _                         = respondUnauthorized schemeName jwtAuthRealm

    schemeName = fromString $ symbolVal $ Proxy @scheme

-- | Middleware to add JWT authentication protection for a
-- handler. Expects the JWT to be available via an authorization
-- header in the format:
--
-- > Authorization: <scheme> <jwt>
--
-- Example usage:
--
-- > optionalJWTAuth' @"<scheme>" cfg handler
--
-- This middleware will not fail if authorization credentials are
-- invalid or missing in the request. Instead the trait attribute is
-- of type Either 'JWTAuthError' 'JWT.ClaimsSet' so that the handler
-- can process the authentication error appropriately.
optionalJWTAuth' :: forall scheme m req e t a. (MonadRouter m, MonadIO m, MonadTime m, KnownSymbol scheme)
                 => JWTAuthConfig m e t
                 -> RequestMiddleware' m req (JWTAuth' Optional scheme m e t : req) a
optionalJWTAuth' JWTAuthConfig{..} handler = authorizationHeader @scheme $ Kleisli $
  transcribe JWTAuth'{..} >=> either absurd (runKleisli handler)


-- | Generate a signed JWT from a JWK and claims
mkJWT :: JWT.MonadRandom m
      => JWT.JWK
      -> Object   -- ^ claim set as a JSON object
      -> m (Either JWT.JWTError JWT.SignedJWT)
mkJWT jwk claims = runExceptT $ do
  alg <- JWT.bestJWSAlg jwk
  let hdr = JWT.newJWSHeader ((), alg)
  case fromJSON (Object claims) of
    Error s         -> throwError $ JWT.JWTClaimsSetDecodeError s
    Success claims' -> JWT.signClaims jwk hdr claims'
