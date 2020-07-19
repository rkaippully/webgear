{-|
Description      : Trait capturing the HTTP method in a request
Copyright        : (c) Raghu Kaippully, 2020
License          : MPL-2.0
Maintainer       : rkaippully@gmail.com
-}
module WebGear.Trait.Method
  ( Method
  , IsStdMethod
  ) where

import qualified Network.HTTP.Types as HTTP
import           WebGear.Trait
import           WebGear.Types


-- | A 'Trait' for capturing the HTTP method of a request
data Method (t :: HTTP.StdMethod)

instance (Monad m, IsStdMethod t) => Trait (Method t) Request m where
  type Val (Method t) Request = HTTP.Method

  check :: Tagged (Method t) Request -> m (Maybe (Request, HTTP.Method))
  check (Tagged r) =
    let
      expected = HTTP.renderStdMethod $ toStdMethod $ Proxy @t
      actual = requestMethod r
    in
      pure $ if expected == actual then Just (r, actual) else Nothing


class IsStdMethod t where
  toStdMethod :: Proxy t -> HTTP.StdMethod

instance IsStdMethod HTTP.GET where
  toStdMethod = const HTTP.GET
instance IsStdMethod HTTP.POST where
  toStdMethod = const HTTP.POST
instance IsStdMethod HTTP.HEAD where
  toStdMethod = const HTTP.HEAD
instance IsStdMethod HTTP.PUT where
  toStdMethod = const HTTP.PUT
instance IsStdMethod HTTP.DELETE where
  toStdMethod = const HTTP.DELETE
instance IsStdMethod HTTP.TRACE where
  toStdMethod = const HTTP.TRACE
instance IsStdMethod HTTP.CONNECT where
  toStdMethod = const HTTP.CONNECT
instance IsStdMethod HTTP.OPTIONS where
  toStdMethod = const HTTP.OPTIONS
instance IsStdMethod HTTP.PATCH where
  toStdMethod = const HTTP.PATCH
