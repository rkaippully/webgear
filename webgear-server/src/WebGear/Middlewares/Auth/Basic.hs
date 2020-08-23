-- |
-- Copyright        : (c) Raghu Kaippully, 2020
-- License          : MPL-2.0
-- Maintainer       : rkaippully@gmail.com
--
-- Basic authentication support.
--
module WebGear.Middlewares.Auth.Basic
  ( BasicAuth
  , Realm (..)
  , Username (..)
  , Password (..)
  , Credentials (..)
  , BasicAuthError (..)
  , basicAuth
  ) where

import Control.Arrow (Kleisli (..))
import Control.Monad (when, (>=>))
import Control.Monad.Except (throwError)
import Data.ByteString (ByteString, intercalate)
import Data.ByteString.Base64 (decodeLenient)
import Data.ByteString.Char8 (split)
import Data.CaseInsensitive (CI, mk)
import Data.String (IsString)
import Data.Tagged (untag)

import WebGear.Route (MonadRouter (..))
import WebGear.Trait (Has (..), Linked, Result (..), Trait (..), probe)
import WebGear.Types (Request, RequestMiddleware, Response (..), requestHeader)
import WebGear.Util (maybeToRight)

import qualified Data.HashMap.Strict as HM
import qualified Network.HTTP.Types as HTTP


-- | Trait for HTTP basic authentication: https://tools.ietf.org/html/rfc7617
data BasicAuth

-- | The protection space for basic authentication
newtype Realm = Realm ByteString
  deriving newtype (Eq, Ord, Show, Read, IsString)

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

-- | Error extracting credentials from an HTTP request
data BasicAuthError = AuthHeaderError        -- ^ Authorization header is missing or badly formatted
                    | AuthSchemeMismatch     -- ^ Authorization scheme is not "Basic"
                    deriving (Eq, Ord, Show, Read)

instance Monad m => Trait BasicAuth Request m where
  type Attribute BasicAuth Request = Credentials
  type Absence BasicAuth Request = BasicAuthError

  toAttribute :: Request -> m (Result BasicAuth Request)
  toAttribute r = pure $ either Refutation (Proof r) $ do
    h <- getAuthHeader r
    (scheme, creds) <- parseAuthHeader h
    when (scheme /= "Basic") $
      throwError AuthSchemeMismatch
    parseCreds creds

type Scheme = CI ByteString
type EncodedPassword = ByteString

getAuthHeader :: Request -> Either BasicAuthError ByteString
getAuthHeader r = maybeToRight AuthHeaderError $ requestHeader "Authorization" r

parseAuthHeader :: ByteString -> Either BasicAuthError (Scheme, EncodedPassword)
parseAuthHeader s =
  case split ' ' s of
    [x, y] -> pure (mk x, y)
    _      -> throwError AuthHeaderError

parseCreds :: EncodedPassword -> Either BasicAuthError Credentials
parseCreds enc =
  case split ':' (decodeLenient enc) of
    []   -> throwError AuthHeaderError
    u:ps -> pure $ Credentials (Username u) (Password $ intercalate ":" ps)

-- | Middleware to add basic authentication protection for a handler.
--
-- Example usage:
--
-- > basicAuth "realm" isValidCredentials handler
--
-- This middleware returns a 401 response if no credentials are found
-- in the request. It returns a 403 response if credentials are
-- present but isValidCredentials returns False.
--
basicAuth :: forall m req a. MonadRouter m
          => Realm
          -> (Credentials -> m Bool)
          -> RequestMiddleware m req (BasicAuth : req) a
basicAuth (Realm realm) credCheck handler = Kleisli $
  probe @BasicAuth >=> either unauthorized (validateCredentials >=> runKleisli handler)
  where
    unauthorized :: BasicAuthError -> m (Response a)
    unauthorized = const $ failHandler $ Response
      { responseStatus  = HTTP.unauthorized401
      , responseHeaders = HM.singleton "WWW-Authenticate" ("Basic realm=\"" <> realm <> "\"")
      , responseBody    = Just "Unauthorized"
      }

    validateCredentials :: Linked (BasicAuth : req) Request
                        -> m (Linked (BasicAuth : req) Request)
    validateCredentials req = do
      valid <- credCheck . untag $ get @BasicAuth req
      if valid
        then pure req
        else failHandler $ Response
               { responseStatus = HTTP.forbidden403
               , responseHeaders = HM.empty
               , responseBody = Just "Forbidden"
               }
