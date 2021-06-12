{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Copyright        : (c) Raghu Kaippully, 2020-2021
-- License          : MPL-2.0
-- Maintainer       : rkaippully@gmail.com
--
-- Basic authentication support.
--
module WebGear.Middlewares.Auth.Basic
  ( BasicAuth' (..)
  , BasicAuth
  , BasicAuthConfig (..)
  , Realm (..)
  , Username (..)
  , Password (..)
  , Credentials (..)
  , BasicAuthError (..)
  , basicAuth
  , optionalBasicAuth
  ) where

import Control.Arrow (Kleisli (..))
import Control.Monad ((>=>))
import Control.Monad.Except (MonadError (throwError))
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString.Base64 (decodeLenient)
import Data.ByteString.Char8 (intercalate, split)
import Data.String (IsString)
import Data.Void (Void, absurd)
import WebGear.Middlewares.Auth.Util (AuthToken (..), AuthorizationHeader, Realm (..),
                                      authorizationHeader, respondUnauthorized)
import WebGear.Modifiers (Existence (..))
import WebGear.Trait (HasTrait (..), Linked, Trait (..), pick, transcribe)
import WebGear.Types (MonadRouter (..), Request, RequestMiddleware', Response, forbidden403)


-- | Trait for HTTP basic authentication: https://tools.ietf.org/html/rfc7617
newtype BasicAuth' (x :: Existence) m e a = BasicAuth'
  { toBasicAttribute :: Credentials -> m (Either e a)
  }

type BasicAuth = BasicAuth' Required

-- | Username for basic authentication. Valid usernames cannot contain
-- \':\' characters.
newtype Username = Username ByteString
  deriving newtype (Eq, Ord, Show, Read, IsString)

-- | Password for basic authentication.
newtype Password = Password ByteString
  deriving newtype (Eq, Ord, Show, Read, IsString)

-- | Basic authentication credentials retrieved from an HTTP request
data Credentials = Credentials
  { credentialsUsername :: !Username
  , credentialsPassword :: !Password
  }
  deriving (Eq, Ord, Show, Read)

-- | Configuration settings for JWT authentication
data BasicAuthConfig m e a = BasicAuthConfig
  { basicAuthRealm   :: Realm
  , toBasicAttribute :: Credentials -> m (Either e a)
  }

data BasicAuthError e = BasicAuthHeaderMissing
                      | BasicAuthSchemeMismatch
                      | BasicAuthCredsBadFormat
                      | BasicAuthAttributeError e
                      deriving (Eq, Show, Read)

parseCreds :: AuthToken "Basic" -> Either (BasicAuthError e) Credentials
parseCreds AuthToken{..} =
  case split ':' (decodeLenient authToken) of
    []   -> throwError BasicAuthCredsBadFormat
    u:ps -> pure $ Credentials (Username u) (Password $ intercalate ":" ps)


instance (HasTrait (AuthorizationHeader "Basic") ts, Monad m) => Trait (BasicAuth' Required m e a) ts Request m where
  type Attribute (BasicAuth' Required m e a) Request = a
  type Absence (BasicAuth' Required m e a) Request = BasicAuthError e

  tryLink :: BasicAuth' Required m e a
          -> Linked ts Request
          -> m (Either (BasicAuthError e) a)
  tryLink BasicAuth'{..} r =
    case pick @(AuthorizationHeader "Basic") (from r) of
      Nothing            -> pure $ Left BasicAuthHeaderMissing
      Just (Left _)      -> pure $ Left BasicAuthSchemeMismatch
      Just (Right token) -> either (pure . Left) validateCreds (parseCreds token)
    where
      validateCreds :: Credentials -> m (Either (BasicAuthError e) a)
      validateCreds creds = first BasicAuthAttributeError <$> toBasicAttribute creds

instance (HasTrait (AuthorizationHeader "Basic") ts, Monad m) => Trait (BasicAuth' Optional m e a) ts Request m where
  type Attribute (BasicAuth' Optional m e a) Request = Either (BasicAuthError e) a
  type Absence (BasicAuth' Optional m e a) Request = Void

  tryLink :: BasicAuth' Optional m e a
          -> Linked ts Request
          -> m (Either Void (Either (BasicAuthError e) a))
  tryLink BasicAuth'{..} r = Right <$> tryLink (BasicAuth'{..} :: BasicAuth' Required m e a) r


-- | Middleware to add basic authentication protection for a handler.
--
-- Example usage:
--
-- > basicAuth cfg handler
--
-- This middleware returns a 401 response if no credentials are found
-- in the request. It returns a 403 response if credentials are
-- present but 'toBasicAttribute' failed to convert that to value of type
-- t.
basicAuth :: forall m req e t a. MonadRouter m
          => BasicAuthConfig m e t
          -> RequestMiddleware' m req (BasicAuth m e t : req) a
basicAuth BasicAuthConfig{..} handler = authorizationHeader @"Basic" $ Kleisli $
  transcribe BasicAuth'{..} >=> either mkError (runKleisli handler)
  where
    mkError :: BasicAuthError e -> m (Response a)
    mkError (BasicAuthAttributeError _) = errorResponse $ forbidden403 "Forbidden"
    mkError _                           = respondUnauthorized "Basic" basicAuthRealm

-- | Middleware to add optional basic authentication protection for a handler.
--
-- Example usage:
--
-- > optionalBasicAuth cfg handler
--
-- This middleware will not fail if credentials are invalid or missing
-- in the request. Instead the trait attribute is of type Either
-- 'BasicAuthError' 'Credentials' so that the handler can process the
-- authentication error appropriately.
optionalBasicAuth :: forall m req e t a. MonadRouter m
                  => BasicAuthConfig m e t
                  -> RequestMiddleware' m req (BasicAuth' Optional m e t : req) a
optionalBasicAuth BasicAuthConfig{..} handler = authorizationHeader @"Basic" $ Kleisli $
  transcribe BasicAuth'{..} >=> either absurd (runKleisli handler)
