{-|
Description      : Trait capturing the route path in a request
Copyright        : (c) Raghu Kaippully, 2020
License          : MPL-2.0
Maintainer       : rkaippully@gmail.com
-}
module WebGear.Trait.Path
  ( Path
  , PathVar
  ) where

import Data.Kind (Type)
import Data.List (stripPrefix)
import Data.List.NonEmpty (toList)
import Data.Proxy (Proxy (..))
import Data.Tagged (Tagged (..))
import Data.Text (pack)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Web.HttpApiData (FromHttpApiData (..))

import WebGear.Trait (Trait (..))
import WebGear.Types (Request, requestPath, setRequestPath)
import WebGear.Util (splitOn)


data Path (s :: Symbol)
data PathVar tag (val :: Type)

instance (KnownSymbol s, Monad m) => Trait (Path s) Request m where
  type Val (Path s) Request = ()

  check :: Request -> m (Maybe (Tagged (Path s) Request, ()))
  check r = do
    let expected = map pack $ toList $ splitOn '/' $ symbolVal $ Proxy @s
        actual = requestPath r
    case stripPrefix expected actual of
      Nothing   -> pure Nothing
      Just rest -> pure $ Just (Tagged (setRequestPath rest r), ())

instance (FromHttpApiData val, Monad m) => Trait (PathVar tag val) Request m where
  type Val (PathVar tag val) Request = val

  check :: Request -> m (Maybe (Tagged (PathVar tag val) Request, val))
  check r =
    case requestPath r of
      []     -> pure Nothing
      (x:xs) ->
        case parseUrlPiece @val x of
          Left _  -> pure Nothing
          Right h -> pure $ Just (Tagged (setRequestPath xs r), h)
