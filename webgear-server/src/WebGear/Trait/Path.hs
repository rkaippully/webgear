{-|
Copyright        : (c) Raghu Kaippully, 2020
License          : MPL-2.0
Maintainer       : rkaippully@gmail.com

Traits related to the route path of a request.
-}
module WebGear.Trait.Path
  ( Path
  , PathVar
  , PathVarFail (..)
  ) where

import Data.Kind (Type)
import Data.List (stripPrefix)
import Data.List.NonEmpty (toList)
import Data.Proxy (Proxy (..))
import Data.Text (Text, pack)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Web.HttpApiData (FromHttpApiData (..))

import WebGear.Trait (CheckResult (..), Trait (..))
import WebGear.Types (Request, pathInfo, setPathInfo)
import WebGear.Util (splitOn)


-- | A path component which is literally matched against the request
-- but discarded after that.
data Path (s :: Symbol)

instance (KnownSymbol s, Monad m) => Trait (Path s) Request m where
  type Val (Path s) Request = ()

  -- | The path that could not be matched
  type Fail (Path s) Request = ()

  check :: Request -> m (CheckResult (Path s) Request)
  check r = pure $
    let expected = map pack $ toList $ splitOn '/' $ symbolVal $ Proxy @s
        actual = pathInfo r
    in
      case stripPrefix expected actual of
        Nothing   -> CheckFail ()
        Just rest -> CheckSuccess (setPathInfo rest r) ()


-- | A path variable that is extracted and converted to a value of
-- type @val@. The @tag@ is usually a type-level symbol (string) to
-- uniquely identify this variable.
data PathVar tag (val :: Type)

-- | Failure to extract a 'PathVar'
data PathVarFail = PathVarNotFound | PathVarParseError Text

instance (FromHttpApiData val, Monad m) => Trait (PathVar tag val) Request m where
  type Val (PathVar tag val) Request = val
  type Fail (PathVar tag val) Request = PathVarFail

  check :: Request -> m (CheckResult (PathVar tag val) Request)
  check r = pure $
    case pathInfo r of
      []     -> CheckFail PathVarNotFound
      (x:xs) ->
        case parseUrlPiece @val x of
          Left e  -> CheckFail $ PathVarParseError e
          Right h -> CheckSuccess (setPathInfo xs r) h
