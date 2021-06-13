-- |
-- Copyright        : (c) Raghu Kaippully, 2020-2021
-- License          : MPL-2.0
-- Maintainer       : rkaippully@gmail.com
--
-- Middlewares related to HTTP headers.
--
module WebGear.Middlewares.Header
  ( -- * Traits
    Header
  , Header' (..)
  , HeaderNotFound (..)
  , HeaderParseError (..)
  , HeaderMatch
  , HeaderMatch' (..)
  , HeaderMismatch (..)

    -- * Middlewares
  , header
  , optionalHeader
  , lenientHeader
  , optionalLenientHeader
  , headerMatch
  , optionalHeaderMatch
  , requestContentTypeHeader
  , addResponseHeader
  ) where

import Control.Arrow (Kleisli (..))
import Control.Monad ((>=>))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.String (fromString)
import Data.Text (Text)
import Data.Void (Void, absurd)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Network.HTTP.Types (HeaderName)
import Text.Printf (printf)
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))
import WebGear.Modifiers (Existence (..), ParseStyle (..))
import WebGear.Trait (Linked, Trait (..), probe, unlink)
import WebGear.Types (MonadRouter (..), Request, RequestMiddleware', Response (..),
                      ResponseMiddleware', badRequest400, requestHeader, setResponseHeader)


-- | A 'Trait' for capturing an HTTP header of specified @name@ and
-- converting it to some type @val@ via 'FromHttpApiData'. The
-- modifiers @e@ and @p@ determine how missing headers and parsing
-- errors are handled. The header name is compared case-insensitively.
data Header' (e :: Existence) (p :: ParseStyle) (name :: Symbol) (val :: Type) = Header'

-- | A 'Trait' for capturing a header with name @name@ in a request or
-- response and convert it to some type @val@ via 'FromHttpApiData'.
type Header (name :: Symbol) (val :: Type) = Header' Required Strict name val

-- | Indicates a missing header
data HeaderNotFound = HeaderNotFound
  deriving stock (Read, Show, Eq)

-- | Error in converting a header
newtype HeaderParseError = HeaderParseError Text
  deriving stock (Read, Show, Eq)

deriveRequestHeader :: (KnownSymbol name, FromHttpApiData val)
                    => Proxy name -> Linked ts Request -> (Maybe (Either Text val) -> r) -> r
deriveRequestHeader proxy req cont =
  let s = fromString $ symbolVal proxy
  in cont $ parseHeader <$> requestHeader s (unlink req)


instance (KnownSymbol name, FromHttpApiData val, Monad m) => Trait (Header' Required Strict name val) ts Request m where
  type Attribute (Header' Required Strict name val) Request = val
  type Absence (Header' Required Strict name val) Request = Either HeaderNotFound HeaderParseError

  tryLink :: Header' Required Strict name val
          -> Linked ts Request
          -> m (Either (Either HeaderNotFound HeaderParseError) val)
  tryLink _ r = pure $ deriveRequestHeader (Proxy @name) r $ \case
    Nothing        -> Left $ Left HeaderNotFound
    Just (Left e)  -> Left $ Right $ HeaderParseError e
    Just (Right x) -> Right x


instance (KnownSymbol name, FromHttpApiData val, Monad m) => Trait (Header' Optional Strict name val) ts Request m where
  type Attribute (Header' Optional Strict name val) Request = Maybe val
  type Absence (Header' Optional Strict name val) Request = HeaderParseError

  tryLink :: Header' Optional Strict name val
          -> Linked ts Request
          -> m (Either HeaderParseError (Maybe val))
  tryLink _ r = pure $ deriveRequestHeader (Proxy @name) r $ \case
    Nothing        -> Right Nothing
    Just (Left e)  -> Left $ HeaderParseError e
    Just (Right x) -> Right $ Just x


instance (KnownSymbol name, FromHttpApiData val, Monad m) => Trait (Header' Required Lenient name val) ts Request m where
  type Attribute (Header' Required Lenient name val) Request = Either Text val
  type Absence (Header' Required Lenient name val) Request = HeaderNotFound

  tryLink :: Header' Required Lenient name val
          -> Linked ts Request
          -> m (Either HeaderNotFound (Either Text val))
  tryLink _ r = pure $ deriveRequestHeader (Proxy @name) r $ \case
    Nothing        -> Left HeaderNotFound
    Just (Left e)  -> Right $ Left e
    Just (Right x) -> Right $ Right x


instance (KnownSymbol name, FromHttpApiData val, Monad m) => Trait (Header' Optional Lenient name val) ts Request m where
  type Attribute (Header' Optional Lenient name val) Request = Maybe (Either Text val)
  type Absence (Header' Optional Lenient name val) Request = Void

  tryLink :: Header' Optional Lenient name val
          -> Linked ts Request
          -> m (Either Void (Maybe (Either Text val)))
  tryLink _ r = pure $ deriveRequestHeader (Proxy @name) r $ \case
    Nothing        -> Right Nothing
    Just (Left e)  -> Right $ Just $ Left e
    Just (Right x) -> Right $ Just $ Right x


-- | A 'Trait' for ensuring that an HTTP header with specified @name@
-- has value @val@. The modifier @e@ determines how missing headers
-- are handled. The header name is compared case-insensitively.
data HeaderMatch' (e :: Existence) (name :: Symbol) (val :: Symbol) = HeaderMatch'

-- | A 'Trait' for ensuring that a header with a specified @name@ has
-- value @val@.
type HeaderMatch (name :: Symbol) (val :: Symbol) = HeaderMatch' Required name val

-- | Failure in extracting a header value
data HeaderMismatch = HeaderMismatch
  { expectedHeader :: ByteString
  , actualHeader   :: ByteString
  }
  deriving stock (Eq, Read, Show)


instance (KnownSymbol name, KnownSymbol val, Monad m) => Trait (HeaderMatch' Required name val) ts Request m where
  type Attribute (HeaderMatch' Required name val) Request = ByteString
  type Absence (HeaderMatch' Required name val) Request = Maybe HeaderMismatch

  tryLink :: HeaderMatch' Required name val
          -> Linked ts Request
          -> m (Either (Maybe HeaderMismatch) ByteString)
  tryLink _ r = pure $
    let
      name = fromString $ symbolVal (Proxy @name)
      expected = fromString $ symbolVal (Proxy @val)
    in
      case requestHeader name (unlink r) of
        Nothing                  -> Left Nothing
        Just hv | hv == expected -> Right hv
                | otherwise      -> Left $ Just HeaderMismatch {expectedHeader = expected, actualHeader = hv}

instance (KnownSymbol name, KnownSymbol val, Monad m) => Trait (HeaderMatch' Optional name val) ts Request m where
  type Attribute (HeaderMatch' Optional name val) Request = Maybe ByteString
  type Absence (HeaderMatch' Optional name val) Request = HeaderMismatch

  tryLink :: HeaderMatch' Optional name val
          -> Linked ts Request
          -> m (Either HeaderMismatch (Maybe ByteString))
  tryLink _ r = pure $
    let
      name = fromString $ symbolVal (Proxy @name)
      expected = fromString $ symbolVal (Proxy @val)
    in
      case requestHeader name (unlink r) of
        Nothing                  -> Right Nothing
        Just hv | hv == expected -> Right $ Just hv
                | otherwise      -> Left $ HeaderMismatch {expectedHeader = expected, actualHeader = hv}


-- | A middleware to extract a header value and convert it to a value
-- of type @val@ using 'FromHttpApiData'.
--
-- Example usage:
--
-- > header @"Content-Length" @Integer handler
--
-- The associated trait attribute has type @val@. A 400 Bad Request
-- response is returned if the header is not found or could not be
-- parsed.
header :: forall name val m req a.
          (KnownSymbol name, FromHttpApiData val, MonadRouter m)
       => RequestMiddleware' m req (Header name val:req) a
header handler = Kleisli $
  probe Header' >=> either (errorResponse . mkError) (runKleisli handler)
  where
    headerName :: String
    headerName = symbolVal $ Proxy @name

    mkError :: Either HeaderNotFound HeaderParseError -> Response LBS.ByteString
    mkError (Left HeaderNotFound) = badRequest400 $ fromString $ printf "Could not find header %s" headerName
    mkError (Right (HeaderParseError _)) = badRequest400 $ fromString $
      printf "Invalid value for header %s" headerName

-- | A middleware to extract a header value and convert it to a value
-- of type @val@ using 'FromHttpApiData'.
--
-- Example usage:
--
-- > optionalHeader @"Content-Length" @Integer handler
--
-- The associated trait attribute has type @Maybe val@; a @Nothing@
-- value indicates that the header is missing from the request. A 400
-- Bad Request response is returned if the header could not be parsed.
optionalHeader :: forall name val m req a.
                  (KnownSymbol name, FromHttpApiData val, MonadRouter m)
               => RequestMiddleware' m req (Header' Optional Strict name val:req) a
optionalHeader handler = Kleisli $
  probe Header' >=> either (errorResponse . mkError) (runKleisli handler)
  where
    headerName :: String
    headerName = symbolVal $ Proxy @name

    mkError :: HeaderParseError -> Response LBS.ByteString
    mkError (HeaderParseError _) = badRequest400 $ fromString $
      printf "Invalid value for header %s" headerName

-- | A middleware to extract a header value and convert it to a value
-- of type @val@ using 'FromHttpApiData'.
--
-- Example usage:
--
-- > lenientHeader @"Content-Length" @Integer handler
--
-- The associated trait attribute has type @Either Text val@. A 400
-- Bad Request reponse is returned if the header is missing. The
-- parsing is done leniently; the trait attribute is set to @Left
-- Text@ in case of parse errors or @Right val@ on success.
lenientHeader :: forall name val m req a.
                 (KnownSymbol name, FromHttpApiData val, MonadRouter m)
              => RequestMiddleware' m req (Header' Required Lenient name val:req) a
lenientHeader handler = Kleisli $
  probe Header' >=> either (errorResponse . mkError) (runKleisli handler)
  where
    headerName :: String
    headerName = symbolVal $ Proxy @name

    mkError :: HeaderNotFound -> Response LBS.ByteString
    mkError HeaderNotFound = badRequest400 $ fromString $ printf "Could not find header %s" headerName

-- | A middleware to extract an optional header value and convert it
-- to a value of type @val@ using 'FromHttpApiData'.
--
-- Example usage:
--
-- > optionalLenientHeader @"Content-Length" @Integer handler
--
-- The associated trait attribute has type @Maybe (Either Text
-- val)@. This middleware never fails.
optionalLenientHeader :: forall name val m req a.
                         (KnownSymbol name, FromHttpApiData val, MonadRouter m)
                      => RequestMiddleware' m req (Header' Optional Lenient name val:req) a
optionalLenientHeader handler = Kleisli $
  probe Header' >=> either absurd (runKleisli handler)

-- | A middleware to ensure that a header in the request has a
-- specific value. Fails the handler with a 400 Bad Request response
-- if the header does not exist or does not match.
headerMatch :: forall name val m req a.
               (KnownSymbol name, KnownSymbol val, MonadRouter m)
            => RequestMiddleware' m req (HeaderMatch name val:req) a
headerMatch handler = Kleisli $
  probe HeaderMatch' >=> either (errorResponse . mkError) (runKleisli handler)
  where
    headerName :: String
    headerName = symbolVal $ Proxy @name

    mkError :: Maybe HeaderMismatch -> Response LBS.ByteString
    mkError Nothing  = badRequest400 $ fromString $ printf "Could not find header %s" headerName
    mkError (Just e) = badRequest400 $ fromString $
      printf "Expected header %s to have value %s but found %s" headerName (show $ expectedHeader e) (show $ actualHeader e)

-- | A middleware to ensure that an optional header in the request has
-- a specific value. Fails the handler with a 400 Bad Request response
-- if the header has a different value.
optionalHeaderMatch :: forall name val m req a.
                       (KnownSymbol name, KnownSymbol val, MonadRouter m)
                    => RequestMiddleware' m req (HeaderMatch' Optional name val:req) a
optionalHeaderMatch handler = Kleisli $
  probe HeaderMatch' >=> either (errorResponse . mkError) (runKleisli handler)
  where
    headerName :: String
    headerName = symbolVal $ Proxy @name

    mkError :: HeaderMismatch -> Response LBS.ByteString
    mkError e = badRequest400 $ fromString $
      printf "Expected header %s to have value %s but found %s" headerName (show $ expectedHeader e) (show $ actualHeader e)

-- | A middleware to check that the Content-Type header in the request
-- has a specific value. It will fail the handler if the header did
-- not match.
--
-- Example usage:
--
-- > requestContentTypeHeader @"application/json" handler
--
requestContentTypeHeader :: forall val m req a. (KnownSymbol val, MonadRouter m)
                         => RequestMiddleware' m req (HeaderMatch "Content-Type" val:req) a
requestContentTypeHeader = headerMatch @"Content-Type" @val

-- | A middleware to create or update a response header.
--
-- Example usage:
--
-- > addResponseHeader "Content-type" "application/json" handler
--
addResponseHeader :: forall t m req a. (ToHttpApiData t, Monad m)
                  => HeaderName -> t -> ResponseMiddleware' m req a a
addResponseHeader name val handler = Kleisli $ runKleisli handler >=> pure . setResponseHeader name (toHeader val)
