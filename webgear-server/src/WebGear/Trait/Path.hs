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

import WebGear.Trait (Trait (..))
import WebGear.Types (Request, requestPath, setRequestPath)

import Web.HttpApiData (FromHttpApiData (..))


data Path (s :: Symbol)
data PathVar tag (val :: Type)

instance (KnownSymbol s, Monad m) => Trait (Path s) Request m where
  type Val (Path s) Request = ()

  check :: Tagged (Path s) Request -> m (Maybe (Request, ()))
  check (Tagged r) = do
    let expected = map toText $ toList $ splitOn '/' $ symbolVal $ Proxy @s
        actual = requestPath r
    case stripPrefix expected actual of
      Nothing   -> pure Nothing
      Just rest -> pure $ Just (setRequestPath rest r, ())

instance (FromHttpApiData val, Monad m) => Trait (PathVar tag val) Request m where
  type Val (PathVar tag val) Request = val

  check :: Tagged (PathVar tag val) Request -> m (Maybe (Request, val))
  check (Tagged r) =
    case requestPath r of
      []     -> pure Nothing
      (x:xs) ->
        case parseUrlPiece @val x of
          Left _  -> pure Nothing
          Right h -> pure $ Just (setRequestPath xs r, h)
