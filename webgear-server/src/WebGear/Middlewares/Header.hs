-- |
-- Copyright        : (c) Raghu Kaippully, 2020
-- License          : MPL-2.0
-- Maintainer       : rkaippully@gmail.com
--
-- Middlewares related to HTTP headers.
module WebGear.Middlewares.Header
  ( Header
  , HeaderError (..)
  , HeaderMatch
  , HeaderMismatch (..)
  , requestContentType
  ) where

import Control.Arrow (Kleisli (..))
import Control.Monad ((>=>))
import Data.ByteString (ByteString)
import Data.HashMap.Strict (fromList)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.String (fromString)
import Data.Text (Text)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Network.HTTP.Types (badRequest400)
import Text.Printf (printf)
import Web.HttpApiData (FromHttpApiData (..))

import WebGear.Route (MonadRouter (..))
import WebGear.Trait (Result (..), Trait (..), linkplus)
import WebGear.Types (Request, RequestMiddleware, Response (..), requestHeader)

import qualified Data.ByteString.Lazy as LBS


-- | A 'Trait' for capturing a header with name @s@ in a request or
-- response and convert it to some type @t@ via 'FromHttpApiData'.
data Header (s :: Symbol) (t :: Type)

-- | Failure in extracting a header value
data HeaderError = HeaderNotFound | HeaderParseError Text
  deriving stock (Read, Show, Eq)

instance (KnownSymbol s, FromHttpApiData t, Monad m) => Trait (Header s t) Request m where
  type Attribute (Header s t) Request = t
  type Absence (Header s t) Request = HeaderError

  prove :: Request -> m (Result (Header s t) Request)
  prove r = pure $
    let s = fromString $ symbolVal (Proxy @s)
    in case parseHeader <$> requestHeader s r of
         Nothing        -> Refutation HeaderNotFound
         Just (Left e)  -> Refutation $ HeaderParseError e
         Just (Right x) -> Proof r x

-- | A 'Trait' for ensuring that a header named @s@ has value @t@.
data HeaderMatch (s :: Symbol) (t :: Symbol)

-- | Failure in extracting a header value
data HeaderMismatch = HeaderMismatch
  { expectedHeader :: ByteString
  , actualHeader   :: Maybe ByteString
  }
  deriving stock (Eq, Read, Show)

instance (KnownSymbol s, KnownSymbol t, Monad m) => Trait (HeaderMatch s t) Request m where
  type Attribute (HeaderMatch s t) Request = ByteString
  type Absence (HeaderMatch s t) Request = HeaderMismatch

  prove :: Request -> m (Result (HeaderMatch s t) Request)
  prove r = pure $
    let
      name = fromString $ symbolVal (Proxy @s)
      expected = fromString $ symbolVal (Proxy @t)
    in
      case requestHeader name r of
        Nothing                  -> Refutation HeaderMismatch {expectedHeader = expected, actualHeader = Nothing}
        Just hv | hv == expected -> Proof r hv
                | otherwise      -> Refutation HeaderMismatch {expectedHeader = expected, actualHeader = Just hv}

-- | A middleware to check that the Content-Type header in the request
-- has a specific value. It will fail the handler if the header did
-- not match.
--
-- Typical usage:
--
-- > requestContentType @"application/json" handler
--
requestContentType :: forall c m req res a. (KnownSymbol c, MonadRouter m)
                   => RequestMiddleware m req (HeaderMatch "Content-Type" c:req) res a
requestContentType handler = Kleisli $
  linkplus @(HeaderMatch "Content-Type" c) >=> either (failHandler . mkError) (runKleisli handler)
  where
    mkError :: HeaderMismatch -> Response LBS.ByteString
    mkError err = Response
                  { respStatus  = badRequest400
                  , respHeaders = fromList []
                  , respBody    = Just $ fromString $
                    case (expectedHeader err, actualHeader err) of
                      (ex, Nothing) -> printf "Expected Content-Type header %s but not found" (show ex)
                      (ex, Just h)  -> printf "Expected Content-Type header %s but found %s" (show ex) (show h)
                  }
