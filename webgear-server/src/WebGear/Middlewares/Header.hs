-- |
-- Copyright        : (c) Raghu Kaippully, 2020
-- License          : MPL-2.0
-- Maintainer       : rkaippully@gmail.com
--
-- Middlewares related to HTTP headers.
--
module WebGear.Middlewares.Header
  ( Header
  , HeaderError (..)
  , HeaderMatch
  , HeaderMismatch (..)
  , requestContentType
  , addResponseHeader
  ) where

import Control.Arrow (Kleisli (..))
import Control.Monad ((>=>))
import Data.ByteString (ByteString)
import Data.HashMap.Strict (fromList)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.String (fromString)
import Data.Tagged (Tagged (..))
import Data.Text (Text)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Network.HTTP.Types (badRequest400)
import Text.Printf (printf)
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))

import WebGear.Route (MonadRouter (..))
import WebGear.Trait (Attachable (..), Result (..), Trait (..), connect, probe)
import WebGear.Types (Request, RequestMiddleware, Response (..), ResponseMiddleware, requestHeader,
                      requestHeaders, responseHeader)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM


-- | A 'Trait' for capturing a header with name @s@ in a request or
-- response and convert it to some type @t@ via 'FromHttpApiData'.
data Header (s :: Symbol) (t :: Type)

-- | Failure in extracting a header value
data HeaderError = HeaderNotFound | HeaderParseError Text
  deriving stock (Read, Show, Eq)

instance (KnownSymbol s, FromHttpApiData t, Monad m) => Trait (Header s t) Request m where
  type Attribute (Header s t) Request = t
  type Absence (Header s t) Request = HeaderError

  derive :: Request -> m (Result (Header s t) Request)
  derive r = pure $
    let s = fromString $ symbolVal (Proxy @s)
    in case parseHeader <$> requestHeader s r of
         Nothing        -> Refutation HeaderNotFound
         Just (Left e)  -> Refutation $ HeaderParseError e
         Just (Right x) -> Proof r x

instance (KnownSymbol s, FromHttpApiData t, ToHttpApiData t, Monad m) => Attachable (Header s t) Request m where
  attach :: t -> Request -> m (Tagged (Header s t) Request)
  attach x r = pure $
    let s = fromString $ symbolVal (Proxy @s)
    in Tagged r { requestHeaders = (s, toHeader x) : requestHeaders r }

instance (KnownSymbol s, FromHttpApiData t, Monad m) => Trait (Header s t) (Response a) m where
  type Attribute (Header s t) (Response a) = t
  type Absence (Header s t) (Response a) = HeaderError

  derive :: Response a -> m (Result (Header s t) (Response a))
  derive r = pure $
    let s = fromString $ symbolVal (Proxy @s)
    in case parseHeader <$> responseHeader s r of
         Nothing        -> Refutation HeaderNotFound
         Just (Left e)  -> Refutation $ HeaderParseError e
         Just (Right x) -> Proof r x

instance (KnownSymbol s, FromHttpApiData t, ToHttpApiData t, Monad m) => Attachable (Header s t) (Response a) m where
  attach :: t -> Response a -> m (Tagged (Header s t) (Response a))
  attach x r = pure $
    let s = fromString $ symbolVal (Proxy @s)
    in Tagged r { responseHeaders = HM.insert s (toHeader x) (responseHeaders r) }

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

  derive :: Request -> m (Result (HeaderMatch s t) Request)
  derive r = pure $
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
-- Example usage:
--
-- > requestContentType @"application/json" handler
--
requestContentType :: forall c m req res a. (KnownSymbol c, MonadRouter m)
                   => RequestMiddleware m req (HeaderMatch "Content-Type" c:req) res a
requestContentType handler = Kleisli $
  probe @(HeaderMatch "Content-Type" c) >=> either (failHandler . mkError) (runKleisli handler)
  where
    mkError :: HeaderMismatch -> Response LBS.ByteString
    mkError err = Response
                  { responseStatus  = badRequest400
                  , responseHeaders = fromList []
                  , responseBody    = Just $ fromString $
                      case (expectedHeader err, actualHeader err) of
                        (ex, Nothing) -> printf "Expected Content-Type header %s but not found" (show ex)
                        (ex, Just h)  -> printf "Expected Content-Type header %s but found %s" (show ex) (show h)
                  }

-- | A middleware to create or update a response header.
--
-- Example usage:
--
-- > addResponseHeader @"Content-type" "application/json" handler
--
addResponseHeader :: forall h t m req res a.
                     (KnownSymbol h, FromHttpApiData t, ToHttpApiData t, Monad m)
                  => t -> ResponseMiddleware m req res (Header h t : res) a
addResponseHeader val handler = Kleisli $ runKleisli handler >=> connect @(Header h t) val
