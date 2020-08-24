-- |
-- Copyright        : (c) Raghu Kaippully, 2020
-- License          : MPL-2.0
-- Maintainer       : rkaippully@gmail.com
--
-- Middlewares for handling query parameters
--
module WebGear.Middlewares.Params
  ( -- * Traits
    QueryParam
  , QueryParam'
  , ParamNotFound (..)
  , ParamParseError (..)

    -- * Middlewares
  , queryParam
  , optionalQueryParam
  , lenientQueryParam
  , optionalLenientQueryParam
  ) where

import Control.Arrow (Kleisli (..))
import Control.Monad ((>=>))
import Data.List (find)
import Data.Proxy (Proxy (..))
import Data.String (fromString)
import Data.Text (Text)
import Data.Void (Void, absurd)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Network.HTTP.Types (queryToQueryText)
import Text.Printf (printf)
import Web.HttpApiData (FromHttpApiData (..))

import WebGear.Modifiers (Existence (..), ParseStyle (..))
import WebGear.Route (MonadRouter (..))
import WebGear.Trait (Result (..), Trait (..), probe)
import WebGear.Types (Request, RequestMiddleware, Response (..), badRequest400, queryString)

import qualified Data.ByteString.Lazy as LBS


-- | Capture a query parameter with a specified @name@ and convert it
-- to a value of type @val@ via 'FromHttpApiData'.
type QueryParam (name :: Symbol) val = QueryParam' Required Strict name val

-- | Capture a query parameter with a specified @name@ and convert it
-- to a value of type @val@ via 'FromHttpApiData'. The type parameter
-- @e@ denotes whether the query parameter is required to be
-- present. The parse style parameter @p@ determines whether the
-- conversion is applied strictly or leniently.
data QueryParam' (e :: Existence) (p :: ParseStyle) (name :: Symbol) val

-- | Indicates a missing query parameter
data ParamNotFound = ParamNotFound
  deriving stock (Read, Show, Eq)

-- | Error in converting a query parameter
newtype ParamParseError = ParamParseError Text
  deriving stock (Read, Show, Eq)

deriveRequestParam :: (KnownSymbol name, FromHttpApiData val)
                    => Proxy name -> Request -> (Maybe (Either Text val) -> r) -> r
deriveRequestParam proxy req cont =
  let name = fromString $ symbolVal proxy
      params = queryToQueryText $ queryString req
  in cont $ parseQueryParam <$> (find ((== name) . fst) params >>= snd)

instance (KnownSymbol name, FromHttpApiData val, Monad m) => Trait (QueryParam' Required Strict name val) Request m where
  type Attribute (QueryParam' Required Strict name val) Request = val
  type Absence (QueryParam' Required Strict name val) Request = Either ParamNotFound ParamParseError

  toAttribute :: Request -> m (Result (QueryParam' Required Strict name val) Request)
  toAttribute r = pure $ deriveRequestParam (Proxy @name) r $ \case
    Nothing        -> Refutation (Left ParamNotFound)
    Just (Left e)  -> Refutation (Right $ ParamParseError e)
    Just (Right x) -> Proof r x

instance (KnownSymbol name, FromHttpApiData val, Monad m) => Trait (QueryParam' Optional Strict name val) Request m where
  type Attribute (QueryParam' Optional Strict name val) Request = Maybe val
  type Absence (QueryParam' Optional Strict name val) Request = ParamParseError

  toAttribute :: Request -> m (Result (QueryParam' Optional Strict name val) Request)
  toAttribute r = pure $ deriveRequestParam (Proxy @name) r $ \case
    Nothing        -> Proof r Nothing
    Just (Left e)  -> Refutation $ ParamParseError e
    Just (Right x) -> Proof r (Just x)

instance (KnownSymbol name, FromHttpApiData val, Monad m) => Trait (QueryParam' Required Lenient name val) Request m where
  type Attribute (QueryParam' Required Lenient name val) Request = Either Text val
  type Absence (QueryParam' Required Lenient name val) Request = ParamNotFound

  toAttribute :: Request -> m (Result (QueryParam' Required Lenient name val) Request)
  toAttribute r = pure $ deriveRequestParam (Proxy @name) r $ \case
    Nothing        -> Refutation ParamNotFound
    Just (Left e)  -> Proof r (Left e)
    Just (Right x) -> Proof r (Right x)

instance (KnownSymbol name, FromHttpApiData val, Monad m) => Trait (QueryParam' Optional Lenient name val) Request m where
  type Attribute (QueryParam' Optional Lenient name val) Request = Maybe (Either Text val)
  type Absence (QueryParam' Optional Lenient name val) Request = Void

  toAttribute :: Request -> m (Result (QueryParam' Optional Lenient name val) Request)
  toAttribute r = pure $ deriveRequestParam (Proxy @name) r $ \case
    Nothing        -> Proof r Nothing
    Just (Left e)  -> Proof r (Just (Left e))
    Just (Right x) -> Proof r (Just (Right x))


-- | A middleware to extract a query parameter and convert it to a
-- value of type @val@ using 'FromHttpApiData'.
--
-- Example usage:
--
-- > queryParam @"limit" @Int handler
--
-- The associated trait attribute has type @val@. This middleware will
-- respond with a 400 Bad Request response if the query parameter is
-- not found or could not be parsed.
queryParam :: forall name val m req a. (KnownSymbol name, FromHttpApiData val, MonadRouter m)
           => RequestMiddleware m req (QueryParam name val:req) a
queryParam handler = Kleisli $ probe @(QueryParam name val) >=> either (failHandler . mkError) (runKleisli handler)
  where
    paramName :: String
    paramName = symbolVal $ Proxy @name

    mkError :: Either ParamNotFound ParamParseError -> Response LBS.ByteString
    mkError err = badRequest400 $ fromString $
      case err of
        Left ParamNotFound        -> printf "Could not find query parameter %s" paramName
        Right (ParamParseError _) -> printf "Invalid value for query parameter %s" paramName

-- | A middleware to extract an optional query parameter and convert
-- it to a value of type @val@ using 'FromHttpApiData'.
--
-- Example usage:
--
-- > optionalQueryParam @"limit" @Int handler
--
-- The associated trait attribute has type @Maybe val@; a @Nothing@
-- value indicates a missing param. A 400 Bad Request response is
-- returned if the query parameter could not be parsed.
optionalQueryParam :: forall name val m req a. (KnownSymbol name, FromHttpApiData val, MonadRouter m)
                   => RequestMiddleware m req (QueryParam' Optional Strict name val:req) a
optionalQueryParam handler = Kleisli $ probe @(QueryParam' Optional Strict name val) >=> either (failHandler . mkError) (runKleisli handler)
  where
    paramName :: String
    paramName = symbolVal $ Proxy @name

    mkError :: ParamParseError -> Response LBS.ByteString
    mkError _ = badRequest400 $ fromString $ printf "Invalid value for query parameter %s" paramName

-- | A middleware to extract a query parameter and convert it to a
-- value of type @val@ using 'FromHttpApiData'.
--
-- Example usage:
--
-- > lenientQueryParam @"limit" @Int handler
--
-- The associated trait attribute has type @Either Text val@. A 400
-- Bad Request reponse is returned if the query parameter is
-- missing. The parsing is done leniently; the trait attribute is set
-- to @Left Text@ in case of parse errors or @Right val@ on success.
lenientQueryParam :: forall name val m req a. (KnownSymbol name, FromHttpApiData val, MonadRouter m)
                  => RequestMiddleware m req (QueryParam' Required Lenient name val:req) a
lenientQueryParam handler = Kleisli $
  probe @(QueryParam' Required Lenient name val) >=> either (failHandler . mkError) (runKleisli handler)
  where
    paramName :: String
    paramName = symbolVal $ Proxy @name

    mkError :: ParamNotFound -> Response LBS.ByteString
    mkError ParamNotFound = badRequest400 $ fromString $ printf "Could not find query parameter %s" paramName

-- | A middleware to extract an optional query parameter and convert it
-- to a value of type @val@ using 'FromHttpApiData'.
--
-- Example usage:
--
-- > optionalLenientHeader @"Content-Length" @Integer handler
--
-- The associated trait attribute has type @Maybe (Either Text
-- val)@. This middleware never fails.
optionalLenientQueryParam :: forall name val m req a. (KnownSymbol name, FromHttpApiData val, MonadRouter m)
                          => RequestMiddleware m req (QueryParam' Optional Lenient name val:req) a
optionalLenientQueryParam handler = Kleisli $
  probe @(QueryParam' Optional Lenient name val) >=> either absurd (runKleisli handler)
