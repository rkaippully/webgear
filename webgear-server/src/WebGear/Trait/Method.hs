-- |
-- Copyright        : (c) Raghu Kaippully, 2020
-- License          : MPL-2.0
-- Maintainer       : rkaippully@gmail.com
--
-- Trait capturing the HTTP method in a request.
module WebGear.Trait.Method
  ( Method
  , IsStdMethod (..)
  , MethodMismatch (..)
  ) where

import Data.Proxy (Proxy (..))

import WebGear.Trait (CheckResult (..), Trait (..))
import WebGear.Types (Request, requestMethod)

import qualified Network.HTTP.Types as HTTP


-- | A 'Trait' for capturing the HTTP method of a request
data Method (t :: HTTP.StdMethod)

-- | Failure to match method against an expected value
data MethodMismatch = MethodMismatch
  { expectedMethod :: HTTP.Method
  , actualMethod   :: HTTP.Method
  }

instance (Monad m, IsStdMethod t) => Trait (Method t) Request m where
  type Val (Method t) Request = HTTP.Method
  type Fail (Method t) Request = MethodMismatch

  check :: Request -> m (CheckResult (Method t) Request)
  check r =
    let
      expected = HTTP.renderStdMethod $ toStdMethod $ Proxy @t
      actual = requestMethod r
    in
      pure $ if expected == actual
             then CheckSuccess r actual
             else CheckFail $ MethodMismatch expected actual


-- | A typeclass implemented by all 'HTTP.StdMethod's to convert them
-- from type level to term level.
class IsStdMethod t where
  -- | Convert @t@ to term level.
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
