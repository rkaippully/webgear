-- |
-- Copyright        : (c) Raghu Kaippully, 2020
-- License          : MPL-2.0
-- Maintainer       : rkaippully@gmail.com
--
-- Middlewares related to HTTP methods.
module WebGear.Middlewares.Method
  ( Method
  , IsStdMethod (..)
  , MethodMismatch (..)
  , method
  ) where

import Control.Arrow (Kleisli (..))
import Control.Monad ((>=>))
import Data.Proxy (Proxy (..))

import WebGear.Trait (Result (..), Trait (..), probe)
import WebGear.Types (MonadRouter (..), Request, RequestMiddleware', requestMethod)

import qualified Network.HTTP.Types as HTTP


-- | A 'Trait' for capturing the HTTP method of a request
data Method (t :: HTTP.StdMethod)

-- | Failure to match method against an expected value
data MethodMismatch = MethodMismatch
  { expectedMethod :: HTTP.Method
  , actualMethod   :: HTTP.Method
  }

instance (IsStdMethod t, Monad m) => Trait (Method t) Request m where
  type Attribute (Method t) Request = ()
  type Absence (Method t) Request = MethodMismatch

  toAttribute :: Request -> m (Result (Method t) Request)
  toAttribute r =
    let
      expected = HTTP.renderStdMethod $ toStdMethod $ Proxy @t
      actual = requestMethod r
    in
      pure $ if expected == actual
             then Found ()
             else NotFound $ MethodMismatch expected actual


-- | A typeclass to map a 'HTTP.StdMethod' from type level to term
-- level.
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

-- | A middleware to check whether the request has a specified HTTP
-- method.
--
-- Typically this would be used with a type application such as:
--
-- > method @GET handler
--
-- It is also idiomatic to use the template haskell quasiquoter
-- 'WebGear.Middlewares.Path.match' in cases where both HTTP method
-- and path needs to be matched.
method :: forall t m req a. (IsStdMethod t, MonadRouter m)
       => RequestMiddleware' m req (Method t:req) a
method handler = Kleisli $ probe @(Method t) >=> either (const rejectRoute) (runKleisli handler)
