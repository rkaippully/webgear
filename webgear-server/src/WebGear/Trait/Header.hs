{-|
Description      : Trait capturing a header
Copyright        : (c) Raghu Kaippully, 2020
License          : MPL-2.0
Maintainer       : rkaippully@gmail.com
-}
module WebGear.Trait.Header
  ( Header
  , HasHeader
  , ContentType
  , HasContentType
  ) where

import Web.HttpApiData (FromHttpApiData (..))

import WebGear.Trait (Trait (..))
import WebGear.Types (Request, requestHeader)


-- | A 'Trait' for capturing a header name 's' in a request or
-- response and convert it to some type 't'.
data Header (s :: Symbol) (t :: Type)

instance (KnownSymbol s, FromHttpApiData t, Monad m) => Trait (Header s t) Request m where
  type Val (Header s t) Request = t

  check :: Tagged (Header s t) Request -> m (Maybe (Request, t))
  check (Tagged r) = pure $ do
    let s = fromString $ symbolVal (Proxy @s)
    hv <- requestHeader s r
    (,) r <$> rightToMaybe (parseHeader hv)

-- | Trait for Content-Type header
type ContentType t = Header "Content-Type" t

-- | A 'Trait' for capturing a header name 's' in a request or
-- response and ensuring that it has value 't'.
data HasHeader (s :: Symbol) (t :: Symbol)

instance (KnownSymbol s, KnownSymbol t, Monad m) => Trait (HasHeader s t) Request m where
  type Val (HasHeader s t) Request = Text

  check :: Tagged (HasHeader s t) Request -> m (Maybe (Request, Text))
  check (Tagged r) = pure $ do
    let s = fromString $ symbolVal (Proxy @s)
        t = fromString $ symbolVal (Proxy @t)
    hv <- rightToMaybe . parseHeader @Text <$> requestHeader s r
    if hv == Just t then Just (r, t) else Nothing

-- | Trait for checking a Content-Type header
type HasContentType t = HasHeader "Content-Type" t
