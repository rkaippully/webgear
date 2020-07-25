{-|
Copyright        : (c) Raghu Kaippully, 2020
License          : MPL-2.0
Maintainer       : rkaippully@gmail.com

Traits related to HTTP headers.
-}
module WebGear.Trait.Header
  ( Header
  , HeaderFail (..)
  , HeaderMatch
  , HeaderMismatch (..)
  ) where

import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.String (fromString)
import Data.Text (Text)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Web.HttpApiData (FromHttpApiData (..))

import WebGear.Trait (CheckResult (..), Trait (..))
import WebGear.Types (Request, requestHeader)


-- | A 'Trait' for capturing a header name @s@ in a request or response
-- and convert it to some type @t@ via 'FromHttpApiData'.
data Header (s :: Symbol) (t :: Type)

-- | Failure in extracting a header value
data HeaderFail = HeaderNotFound | HeaderParseError Text

instance (KnownSymbol s, FromHttpApiData t, Monad m) => Trait (Header s t) Request m where
  type Val (Header s t) Request = t
  type Fail (Header s t) Request = HeaderFail

  check :: Request -> m (CheckResult (Header s t) Request)
  check r = pure $
    let s = fromString $ symbolVal (Proxy @s)
    in case parseHeader <$> requestHeader s r of
         Nothing        -> CheckFail HeaderNotFound
         Just (Left e)  -> CheckFail $ HeaderParseError e
         Just (Right x) -> CheckSuccess r x

-- | A 'Trait' for ensuring that a header named @s@ has value @t@.
data HeaderMatch (s :: Symbol) (t :: Symbol)

-- | Failure in extracting a header value
data HeaderMismatch = HeaderMismatch
  { expectedHeader :: ByteString
  , actualHeader   :: Maybe ByteString
  }

instance (KnownSymbol s, KnownSymbol t, Monad m) => Trait (HeaderMatch s t) Request m where
  type Val (HeaderMatch s t) Request = ByteString
  type Fail (HeaderMatch s t) Request = HeaderMismatch

  check :: Request -> m (CheckResult (HeaderMatch s t) Request)
  check r = pure $
    let
      name = fromString $ symbolVal (Proxy @s)
      expected = fromString $ symbolVal (Proxy @t)
    in
      case requestHeader name r of
        Nothing                  -> CheckFail HeaderMismatch
                                      {expectedHeader = expected, actualHeader = Nothing}
        Just hv | hv == expected -> CheckSuccess r hv
                | otherwise      -> CheckFail HeaderMismatch
                                      {expectedHeader = expected, actualHeader = Just hv}
