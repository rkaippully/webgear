-- |
-- Copyright        : (c) Raghu Kaippully, 2020
-- License          : MPL-2.0
-- Maintainer       : rkaippully@gmail.com
--
-- Middlewares related to HTTP headers.
--
module WebGear.Middlewares.Header
  ( -- * Traits
    Header
  , Header'
  , HeaderError (..)
  , HeaderMatch
  , HeaderMatch'
  , HeaderMismatch (..)

    -- * Middlewares
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
import Data.Text (Text)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Network.HTTP.Types (HeaderName, badRequest400)
import Text.Printf (printf)
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))

import WebGear.Modifiers (Existence (..), ParseStyle (..))
import WebGear.Route (MonadRouter (..))
import WebGear.Trait (Result (..), Trait (..), probe)
import WebGear.Types (Request, RequestMiddleware, Response (..), ResponseMiddleware, requestHeader,
                      responseHeader, setResponseHeader)

import qualified Data.ByteString.Lazy as LBS


-- | A 'Trait' for capturing an HTTP header of specified @name@ and
-- converting it to some type @val@ via 'FromHttpApiData'. The
-- modifiers @e@ and @p@ determine how missing headers and parsing
-- errors are handled. The header name is compared case-insensitively.
data Header' (e :: Existence) (p :: ParseStyle) (name :: Symbol) (val :: Type)

-- | A 'Trait' for capturing a header with name @name@ in a request or
-- response and convert it to some type @val@ via 'FromHttpApiData'.
type Header (name :: Symbol) (val :: Type) = Header' Required Strict name val

-- | Failure in extracting a header value
data HeaderError = HeaderNotFound | HeaderParseError Text
  deriving stock (Read, Show, Eq)

deriveRequestHeader :: (KnownSymbol name, FromHttpApiData val)
                    => Proxy name -> Request -> (Maybe (Either Text val) -> r) -> r
deriveRequestHeader proxy req cont =
  let s = fromString $ symbolVal proxy
  in cont $ parseHeader <$> requestHeader s req

deriveResponseHeader :: (KnownSymbol name, FromHttpApiData val)
                    => Proxy name -> Response a -> (Maybe (Either Text val) -> r) -> r
deriveResponseHeader proxy res cont =
  let s = fromString $ symbolVal proxy
  in cont $ parseHeader <$> responseHeader s res


instance (KnownSymbol name, FromHttpApiData val, Monad m) => Trait (Header' Required Strict name val) Request m where
  type Attribute (Header' Required Strict name val) Request = val
  type Absence (Header' Required Strict name val) Request = HeaderError

  toAttribute :: Request -> m (Result (Header' Required Strict name val) Request)
  toAttribute r = pure $ deriveRequestHeader (Proxy @name) r $ \case
    Nothing        -> Refutation HeaderNotFound
    Just (Left e)  -> Refutation $ HeaderParseError e
    Just (Right x) -> Proof r x


instance (KnownSymbol name, FromHttpApiData val, Monad m) => Trait (Header' Optional Strict name val) Request m where
  type Attribute (Header' Optional Strict name val) Request = Maybe val
  type Absence (Header' Optional Strict name val) Request = HeaderError

  toAttribute :: Request -> m (Result (Header' Optional Strict name val) Request)
  toAttribute r = pure $ deriveRequestHeader (Proxy @name) r $ \case
    Nothing        -> Proof r Nothing
    Just (Left e)  -> Refutation $ HeaderParseError e
    Just (Right x) -> Proof r (Just x)


instance (KnownSymbol name, FromHttpApiData val, Monad m) => Trait (Header' Required Lenient name val) Request m where
  type Attribute (Header' Required Lenient name val) Request = Either Text val
  type Absence (Header' Required Lenient name val) Request = HeaderError

  toAttribute :: Request -> m (Result (Header' Required Lenient name val) Request)
  toAttribute r = pure $ deriveRequestHeader (Proxy @name) r $ \case
    Nothing        -> Refutation HeaderNotFound
    Just (Left e)  -> Proof r (Left e)
    Just (Right x) -> Proof r (Right x)


instance (KnownSymbol name, FromHttpApiData val, Monad m) => Trait (Header' Optional Lenient name val) Request m where
  type Attribute (Header' Optional Lenient name val) Request = Maybe (Either Text val)
  type Absence (Header' Optional Lenient name val) Request = ()

  toAttribute :: Request -> m (Result (Header' Optional Lenient name val) Request)
  toAttribute r = pure $ deriveRequestHeader (Proxy @name) r $ \case
    Nothing        -> Proof r Nothing
    Just (Left e)  -> Proof r (Just (Left e))
    Just (Right x) -> Proof r (Just (Right x))


instance (KnownSymbol name, FromHttpApiData val, Monad m) => Trait (Header' Required Strict name val) (Response a) m where
  type Attribute (Header' Required Strict name val) (Response a) = val
  type Absence (Header' Required Strict name val) (Response a) = HeaderError

  toAttribute :: Response a -> m (Result (Header' Required Strict name val) (Response a))
  toAttribute r = pure $ deriveResponseHeader (Proxy @name) r $ \case
    Nothing        -> Refutation HeaderNotFound
    Just (Left e)  -> Refutation $ HeaderParseError e
    Just (Right x) -> Proof r x


instance (KnownSymbol name, FromHttpApiData val, Monad m) => Trait (Header' Optional Strict name val) (Response a) m where
  type Attribute (Header' Optional Strict name val) (Response a) = Maybe val
  type Absence (Header' Optional Strict name val) (Response a) = HeaderError

  toAttribute :: Response a -> m (Result (Header' Optional Strict name val) (Response a))
  toAttribute r = pure $ deriveResponseHeader (Proxy @name) r $ \case
    Nothing        -> Proof r Nothing
    Just (Left e)  -> Refutation $ HeaderParseError e
    Just (Right x) -> Proof r (Just x)


instance (KnownSymbol name, FromHttpApiData val, Monad m) => Trait (Header' Required Lenient name val) (Response a) m where
  type Attribute (Header' Required Lenient name val) (Response a) = Either Text val
  type Absence (Header' Required Lenient name val) (Response a) = HeaderError

  toAttribute :: Response a -> m (Result (Header' Required Lenient name val) (Response a))
  toAttribute r = pure $ deriveResponseHeader (Proxy @name) r $ \case
    Nothing        -> Refutation HeaderNotFound
    Just (Left e)  -> Proof r (Left e)
    Just (Right x) -> Proof r (Right x)


instance (KnownSymbol name, FromHttpApiData val, Monad m) => Trait (Header' Optional Lenient name val) (Response a) m where
  type Attribute (Header' Optional Lenient name val) (Response a) = Maybe (Either Text val)
  type Absence (Header' Optional Lenient name val) (Response a) = ()

  toAttribute :: Response a -> m (Result (Header' Optional Lenient name val) (Response a))
  toAttribute r = pure $ deriveResponseHeader (Proxy @name) r $ \case
    Nothing        -> Proof r Nothing
    Just (Left e)  -> Proof r (Just (Left e))
    Just (Right x) -> Proof r (Just (Right x))


-- | A 'Trait' for ensuring that an HTTP header with specified @name@
-- has value @val@. The modifier @e@ determines how missing headers
-- are handled. The header name is compared case-insensitively.
data HeaderMatch' (e :: Existence) (name :: Symbol) (val :: Symbol)

-- | A 'Trait' for ensuring that a header with a specified @name@ has
-- value @val@.
type HeaderMatch (name :: Symbol) (val :: Symbol) = HeaderMatch' Required name val

-- | Failure in extracting a header value
data HeaderMismatch = HeaderMismatch
  { expectedHeader :: ByteString
  , actualHeader   :: Maybe ByteString
  }
  deriving stock (Eq, Read, Show)


instance (KnownSymbol name, KnownSymbol val, Monad m) => Trait (HeaderMatch' Required name val) Request m where
  type Attribute (HeaderMatch' Required name val) Request = ()
  type Absence (HeaderMatch' Required name val) Request = HeaderMismatch

  toAttribute :: Request -> m (Result (HeaderMatch' Required name val) Request)
  toAttribute r = pure $
    let
      name = fromString $ symbolVal (Proxy @name)
      expected = fromString $ symbolVal (Proxy @val)
    in
      case requestHeader name r of
        Nothing                  -> Refutation HeaderMismatch {expectedHeader = expected, actualHeader = Nothing}
        Just hv | hv == expected -> Proof r ()
                | otherwise      -> Refutation HeaderMismatch {expectedHeader = expected, actualHeader = Just hv}


instance (KnownSymbol name, KnownSymbol val, Monad m) => Trait (HeaderMatch' Optional name val) Request m where
  type Attribute (HeaderMatch' Optional name val) Request = Maybe ()
  type Absence (HeaderMatch' Optional name val) Request = HeaderMismatch

  toAttribute :: Request -> m (Result (HeaderMatch' Optional name val) Request)
  toAttribute r = pure $
    let
      name = fromString $ symbolVal (Proxy @name)
      expected = fromString $ symbolVal (Proxy @val)
    in
      case requestHeader name r of
        Nothing                  -> Proof r Nothing
        Just hv | hv == expected -> Proof r (Just ())
                | otherwise      -> Refutation HeaderMismatch {expectedHeader = expected, actualHeader = Just hv}

-- | A middleware to check that the Content-Type header in the request
-- has a specific value. It will fail the handler if the header did
-- not match.
--
-- Example usage:
--
-- > requestContentType @"application/json" handler
--
requestContentType :: forall c m req a. (KnownSymbol c, MonadRouter m)
                   => RequestMiddleware m req (HeaderMatch "Content-Type" c:req) a
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
-- > addResponseHeader "Content-type" "application/json" handler
--
addResponseHeader :: forall t m req a. (ToHttpApiData t, Monad m)
                  => HeaderName -> t -> ResponseMiddleware m req a a
addResponseHeader name val handler = Kleisli $ runKleisli handler >=> pure . setResponseHeader name (toHeader val)
