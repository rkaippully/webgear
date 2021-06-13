-- |
-- Copyright        : (c) Raghu Kaippully, 2020-2021
-- License          : MPL-2.0
-- Maintainer       : rkaippully@gmail.com
--
-- Middlewares for handling query parameters
--
module WebGear.Middlewares.Params
  ( -- * Traits
    QueryParam
  , QueryParam' (..)
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
import qualified Data.ByteString.Lazy as LBS
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
import WebGear.Trait (Linked, Trait (..), probe, unlink)
import WebGear.Types (MonadRouter (..), Request, RequestMiddleware', Response (..), badRequest400,
                      queryString)


-- | Capture a query parameter with a specified @name@ and convert it
-- to a value of type @val@ via 'FromHttpApiData'.
type QueryParam (name :: Symbol) val = QueryParam' Required Strict name val

-- | Capture a query parameter with a specified @name@ and convert it
-- to a value of type @val@ via 'FromHttpApiData'. The type parameter
-- @e@ denotes whether the query parameter is required to be
-- present. The parse style parameter @p@ determines whether the
-- conversion is applied strictly or leniently.
data QueryParam' (e :: Existence) (p :: ParseStyle) (name :: Symbol) val = QueryParam'

-- | Indicates a missing query parameter
data ParamNotFound = ParamNotFound
  deriving stock (Read, Show, Eq)

-- | Error in converting a query parameter
newtype ParamParseError = ParamParseError Text
  deriving stock (Read, Show, Eq)

deriveRequestParam :: (KnownSymbol name, FromHttpApiData val)
                    => Proxy name -> Linked ts Request -> (Maybe (Either Text val) -> r) -> r
deriveRequestParam proxy req cont =
  let name = fromString $ symbolVal proxy
      params = queryToQueryText $ queryString $ unlink req
  in cont $ parseQueryParam <$> (find ((== name) . fst) params >>= snd)

instance (KnownSymbol name, FromHttpApiData val, Monad m) => Trait (QueryParam' Required Strict name val) ts Request m where
  type Attribute (QueryParam' Required Strict name val) Request = val
  type Absence (QueryParam' Required Strict name val) Request = Either ParamNotFound ParamParseError

  tryLink :: QueryParam' Required Strict name val
          -> Linked ts Request
          -> m (Either (Either ParamNotFound ParamParseError) val)
  tryLink _ r = pure $ deriveRequestParam (Proxy @name) r $ \case
    Nothing        -> Left $ Left ParamNotFound
    Just (Left e)  -> Left $ Right $ ParamParseError e
    Just (Right x) -> Right x

instance (KnownSymbol name, FromHttpApiData val, Monad m) => Trait (QueryParam' Optional Strict name val) ts Request m where
  type Attribute (QueryParam' Optional Strict name val) Request = Maybe val
  type Absence (QueryParam' Optional Strict name val) Request = ParamParseError

  tryLink :: QueryParam' Optional Strict name val
          -> Linked ts Request
          -> m (Either ParamParseError (Maybe val))
  tryLink _ r = pure $ deriveRequestParam (Proxy @name) r $ \case
    Nothing        -> Right Nothing
    Just (Left e)  -> Left $ ParamParseError e
    Just (Right x) -> Right $ Just x

instance (KnownSymbol name, FromHttpApiData val, Monad m) => Trait (QueryParam' Required Lenient name val) ts Request m where
  type Attribute (QueryParam' Required Lenient name val) Request = Either Text val
  type Absence (QueryParam' Required Lenient name val) Request = ParamNotFound

  tryLink :: QueryParam' Required Lenient name val
          -> Linked ts Request
          -> m (Either ParamNotFound (Either Text val))
  tryLink _ r = pure $ deriveRequestParam (Proxy @name) r $ \case
    Nothing        -> Left ParamNotFound
    Just (Left e)  -> Right $ Left e
    Just (Right x) -> Right $ Right x

instance (KnownSymbol name, FromHttpApiData val, Monad m) => Trait (QueryParam' Optional Lenient name val) ts Request m where
  type Attribute (QueryParam' Optional Lenient name val) Request = Maybe (Either Text val)
  type Absence (QueryParam' Optional Lenient name val) Request = Void

  tryLink :: QueryParam' Optional Lenient name val
          -> Linked ts Request
          -> m (Either Void (Maybe (Either Text val)))
  tryLink _ r = pure $ deriveRequestParam (Proxy @name) r $ \case
    Nothing        -> Right Nothing
    Just (Left e)  -> Right $ Just $ Left e
    Just (Right x) -> Right $ Just $ Right x


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
           => RequestMiddleware' m req (QueryParam name val:req) a
queryParam handler = Kleisli $
  probe QueryParam' >=> either (errorResponse . mkError) (runKleisli handler)
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
                   => RequestMiddleware' m req (QueryParam' Optional Strict name val:req) a
optionalQueryParam handler = Kleisli $
  probe QueryParam' >=> either (errorResponse . mkError) (runKleisli handler)
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
                  => RequestMiddleware' m req (QueryParam' Required Lenient name val:req) a
lenientQueryParam handler = Kleisli $
  probe QueryParam' >=> either (errorResponse . mkError) (runKleisli handler)
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
                          => RequestMiddleware' m req (QueryParam' Optional Lenient name val:req) a
optionalLenientQueryParam handler = Kleisli $
  probe QueryParam' >=> either absurd (runKleisli handler)
