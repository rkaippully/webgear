-- |
-- Copyright        : (c) Raghu Kaippully, 2021
-- License          : MPL-2.0
-- Maintainer       : rkaippully@gmail.com
--
-- Util types and functions related to authorization.
--
module WebGear.Middlewares.Auth.Util
  ( AuthorizationHeader
  , authorizationHeader
  , Realm (..)
  , AuthToken (..)
  , respondUnauthorized
  ) where

import Data.ByteString (ByteString, drop)
import Data.ByteString.Char8 (break)
import Data.CaseInsensitive (CI, mk, original)
import Data.Proxy (Proxy (..))
import Data.String (IsString (..))
import Data.Text.Encoding (encodeUtf8)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Prelude hiding (break, drop)
import Web.HttpApiData (FromHttpApiData (..))
import WebGear.Middlewares.Header (Header', optionalLenientHeader)
import WebGear.Modifiers (Existence (..), ParseStyle (..))
import WebGear.Types (MonadRouter (errorResponse), RequestMiddleware', Response, setResponseHeader,
                      unauthorized401)


-- | Header trait for authorization
type AuthorizationHeader scheme = Header' Optional Lenient "Authorization" (AuthToken scheme)

authorizationHeader :: forall scheme m req a. (KnownSymbol scheme, MonadRouter m)
                    => RequestMiddleware' m req (AuthorizationHeader scheme:req) a
authorizationHeader = optionalLenientHeader @"Authorization" @(AuthToken scheme)

-- | The protection space for authentication
newtype Realm = Realm ByteString
  deriving newtype (Eq, Ord, Show, Read, IsString)

data AuthToken (scheme :: Symbol) = AuthToken
  { authScheme :: CI ByteString
  , authToken  :: ByteString
  }

instance KnownSymbol scheme => FromHttpApiData (AuthToken scheme) where
  parseUrlPiece = parseHeader . encodeUtf8

  parseHeader hdr =
    case break (== ' ') hdr of
      (scm, tok) ->
        let
          actualScheme = mk scm
          expectedScheme = fromString $ symbolVal $ Proxy @scheme
        in
          if actualScheme == expectedScheme
          then Right (AuthToken actualScheme (drop 1 tok))
          else Left "scheme mismatch"

respondUnauthorized :: MonadRouter m => CI ByteString -> Realm -> m (Response a)
respondUnauthorized scheme (Realm realm) = errorResponse
  $ setResponseHeader "WWW-Authenticate" (original scheme <> " realm=\"" <> realm <> "\"")
  $ unauthorized401 "Unauthorized"
